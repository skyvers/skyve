package modules.admin.Audit.models;

import static java.util.stream.Collectors.toCollection;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.SortedMap;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.web.SortParameter;

import com.google.common.base.Stopwatch;

import modules.admin.Audit.job.IndexArchivesJob;
import modules.admin.Audit.job.support.AuditDocumentConverter;
import modules.admin.domain.Audit;

public class ArchivedAuditListModel<U extends Bean> extends ListModel<U> {

    private final Logger logger = LogManager.getLogger();

    private org.skyve.metadata.model.document.Document drivingDocument;
    private List<MetaDataQueryColumn> columns;
    private Set<String> projections = new LinkedHashSet<>();

    private LuceneFilter filter = new LuceneFilter();

    @Override
    public void postConstruct(Customer customer, boolean runtime) {
        drivingDocument = customer.getModule(Audit.MODULE_NAME)
                                  .getDocument(customer, Audit.DOCUMENT_NAME);

        List<String> bindings = List.of(Audit.timestampPropertyName,
                Audit.userNamePropertyName,
                Audit.operationPropertyName,
                Audit.auditModuleNamePropertyName,
                Audit.auditDocumentNamePropertyName,
                Audit.auditBizKeyPropertyName,
                Audit.auditBizIdPropertyName);

        // Columns
        columns = bindings.stream()
                          .map(this::createColumn)
                          .collect(toCollection(ArrayList::new));

        // Projections
        projections.add(Bean.DOCUMENT_ID);
        projections.add(PersistentBean.LOCK_NAME);
        projections.add(PersistentBean.TAGGED_NAME);
        projections.add(PersistentBean.FLAG_COMMENT_NAME);
        projections.add(Bean.BIZ_KEY);

        bindings.forEach(projections::add);
    }

    private MetaDataQueryColumn createColumn(String binding) {
        MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
        column.setBinding(binding);
        column.setSortable(true);

        if (Audit.auditBizIdPropertyName.equals(binding)) {
            column.setHidden(true);
        }

        return column;
    }

    @Override
    public String getDescription() {
        return "Query the lucene index etc";
    }

    @Override
    public org.skyve.metadata.model.document.Document getDrivingDocument() {
        return drivingDocument;
    }

    @Override
    public List<MetaDataQueryColumn> getColumns() {
        return columns;
    }

    @Override
    public Set<String> getProjections() {
        return projections;
    }

    @Override
    public Filter getFilter() {

        return filter;
    }

    @Override
    public Filter newFilter() {
        return filter = new LuceneFilter();
    }

    @Override
    public void putParameter(String name, Object value) {

        // unused?
    }

    @Override
    public Page fetch() throws Exception {

        logger.debug("Executing fetch, filter={}, start={}, end={}", filter, getStartRow(), getEndRow());
        Query query = filter.toQuery();

        try {
            Result<Bean> queryResults = executeQuery(query);

            Page p = new Page();
            p.setTotalRows(queryResults.totalRowCount());
            p.setRows(queryResults.rows());
            p.setSummary(createSummary(queryResults.totalRowCount()));

            return p;
        } catch (IndexNotFoundException e) {
            logger.atWarn()
                  .withThrowable(e)
                  .log("No index found, returning empty Page");

            Page p = emptyPage();
            return p;
        }
    }

    public Page emptyPage() {
        Page p = new Page();
        p.setRows(new ArrayList<>());
        p.setSummary(createSummary(0));
        return p;
    }

    private DynamicBean createSummary(long rowCount) {
        HashMap<String, Object> props = new HashMap<>();
        props.put(Bean.DOCUMENT_ID, UUID.randomUUID()
                                        .toString());
        props.put(PersistentBean.FLAG_COMMENT_NAME, "");

        if (getSummary() != null) {
            if (AggregateFunction.Count == getSummary()) {

                for (MetaDataQueryColumn column : getColumns()) {
                    String binding = column.getBinding();

                    props.put(binding, rowCount);
                }
            } else {
                // TODO can we even support the other aggregate types, possibly not
                logger.warn("Aggregate function {} not supported by {}", getSummary(), this);
            }
        }

        return new DynamicBean("admin", "Audit", props);
    }

    private Result<Bean> executeQuery(Query query) throws IOException {

        Stopwatch t = Stopwatch.createStarted();

        Path auditArchiveIndexPath = IndexArchivesJob.getIndexPath();
        logger.debug("Using index at {}", auditArchiveIndexPath);
        try (Directory directory = FSDirectory.open(auditArchiveIndexPath);
                DirectoryReader ireader = DirectoryReader.open(directory)) {

            IndexSearcher isearcher = new IndexSearcher(ireader);
            Result<Document> queryResults = doQuery(ireader, isearcher, query);

            List<Bean> convertedResults = queryResults.rows()
                                                      .stream()
                                                      .map(this::convertToBean)
                                                      .filter(Objects::nonNull)
                                                      .collect(toCollection(ArrayList::new));
            logger.debug("Got {} results of {}; took {}", convertedResults.size(), queryResults.totalRowCount, t);

            return new Result<>(convertedResults, queryResults.totalRowCount);
        }
    }

    private DynamicBean convertToBean(Document luceneDoc) {

        Map<String, Object> props = new HashMap<>();
        // TODO this should probably live adjacent to or in AuditDocumentConverter

        Consumer<String> putStringField = binding -> {
            props.put(binding, luceneDoc.get(binding));
        };

        Stream.of(
                Bean.DOCUMENT_ID,
                Audit.userNamePropertyName,
                Bean.USER_ID,
                Audit.operationPropertyName,
                Audit.auditModuleNamePropertyName,
                Audit.auditDocumentNamePropertyName,
                Audit.auditBizIdPropertyName,
                Audit.auditBizKeyPropertyName)
              .forEach(putStringField);

        String dateStr = luceneDoc.get(Audit.timestampPropertyName);
        props.put(Audit.timestampPropertyName, new Timestamp(AuditDocumentConverter.stringToDate(dateStr)));

        return new DynamicBean("admin", "Audit", props);
    }

    private Result<Document> doQuery(DirectoryReader ireader, IndexSearcher isearcher, Query query) throws IOException {

        int maxResults = getEndRow();
        TopDocs td = isearcher.search(query, maxResults, getSort());

        // Slice out only the page being requested
        ScoreDoc[] resultSubset = ArrayUtils.subarray(td.scoreDocs, getStartRow(), getEndRow());

        List<Document> results = new ArrayList<>(getEndRow() - getStartRow());
        Result<Document> r = new Result<>(results, td.totalHits.value);

        for (ScoreDoc score : resultSubset) {

            logger.trace("Reading stored fields for doc {}", score.doc);
            Document doc = ireader.storedFields()
                                  .document(score.doc);

            results.add(doc);
        }

        return r;
    }

    private Sort getSort() {

        SortParameter[] params = getSortParameters();
        if (params == null || params.length == 0) {
            return Sort.RELEVANCE;
        }

        logger.debug("SortParameters: {}", () -> Arrays.asList(params));

        List<SortField> sortFields = new ArrayList<>(params.length);

        for (SortParameter sp : params) {

            boolean reverse = sp.getDirection() == SortDirection.descending;
            String binding = AuditDocumentConverter.toSortBinding(sp.getBy());
            SortField sf = new SortField(binding, Type.STRING, reverse);

            logger.debug("Sorting by {}", sf);

            sortFields.add(sf);
        }

        return new Sort(sortFields.toArray(new SortField[0]));
    }

    private static record Result<U>(List<U> rows, long totalRowCount) {
    }

    @Override
    public AutoClosingIterable<Bean> iterate() throws Exception {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void remove(String bizId) throws Exception {
        throw new UnsupportedOperationException();
    }

}
