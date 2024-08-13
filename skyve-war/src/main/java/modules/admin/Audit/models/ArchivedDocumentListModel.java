package modules.admin.Audit.models;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.SortedMap;
import java.util.UUID;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.TotalHits;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.Util;
import org.skyve.web.SortParameter;

import com.google.common.base.Stopwatch;

public abstract class ArchivedDocumentListModel<U extends Bean> extends ListModel<U> {

    private final Logger logger = LogManager.getLogger();

    private LuceneFilter filter = new LuceneFilter();

    @Override
    public void postConstruct(Customer customer, boolean runtime) {
        //
    }

    @Override
    public String getDescription() {
        return "The list of all Audits.";
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

        try {
            Result queryResults = executeQuery();

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
                // We probably can't support the other aggregations, and
                // they don't really make sense for the Audit list model
                logger.warn("Aggregate function {} not supported by {}", getSummary(), this);
            }
        }

        return new DynamicBean("admin", "Audit", props);
    }

    private Result executeQuery() throws IOException {

        Stopwatch t = Stopwatch.createStarted();

        List<Bean> rows = new ArrayList<>(getEndRow() - getStartRow());

        try (LuceneResultsIterable lri = this.new LuceneResultsIterable(getStartRow(), getEndRow())) {

            TotalHits hits = lri.totalHits();

            lri.iterator()
               .forEachRemaining(rows::add);

            logger.debug("Got {} results of {}; took {}", rows.size(), lri.totalHits(), t);

            return new Result(rows, hits.value);
        }
    }

    protected Sort getSort() {

        SortParameter[] params = getSortParameters();
        if (params == null || params.length == 0) {
            Sort defaultSort = getDefaultSort();
            logger.debug("No sorting defined, using {}", defaultSort);
            return defaultSort;
        }

        logger.debug("SortParameters: {}", () -> Arrays.asList(params));

        List<SortField> sortFields = new ArrayList<>(params.length);

        for (SortParameter sp : params) {

            boolean reverse = sp.getDirection() == SortDirection.descending;
            String binding = toSortBinding(sp.getBy());
            SortField sf = new SortField(binding, Type.STRING, reverse);

            logger.debug("Sorting by {}", sf);

            sortFields.add(sf);
        }

        return new Sort(sortFields.toArray(new SortField[0]));
    }

    /**
     * Default to sorting by relevance (i.e. Lucene score). Can be overridden by subclasses as needed.
     * 
     * @return
     */
    protected Sort getDefaultSort() {
        return Sort.RELEVANCE;
    }

    /**
     * Convert the given binding into the sort field binding that will be used with
     * Lucene (most likely <em>${binding}_sort</em>); this is determined when converting
     * the Skyve document into a Lucene document in the relevant DocumentConverter.
     * See <em>DocumentConverter.toSortBinding(String)</em> for more info.
     * 
     * @param binding
     * @return
     */
    protected abstract String toSortBinding(String binding);

    /**
     * Convert the supplied Lucene Document into a skyve Bean.
     * <p>
     * NB DynamicBean is suitable, and likely to be easier that constructing a whole
     * Bean instance.
     * 
     * @param luceneDoc
     * @return
     */
    protected abstract Bean convertToBean(Document luceneDoc);

    /**
     * Tuple record for ferrying query results around
     */
    private static record Result(List<Bean> rows, long totalRowCount) {
    }

    @Override
    public AutoClosingIterable<Bean> iterate() throws Exception {

        return this.new LuceneResultsIterable(0, Integer.MAX_VALUE);
    }

    @Override
    public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public void remove(String bizId) throws Exception {
        throw new UnsupportedOperationException();
    }

    @Override
    public org.skyve.metadata.model.document.Document getDrivingDocument() {
        Customer customer = CORE.getUser()
                                .getCustomer();

        return customer.getModule(getModule())
                       .getDocument(customer, getDocument());
    }

    protected abstract String getModule();

    protected abstract String getDocument();

    /**
     * Get the document config for the configured module+document from the application config (via
     * <em>Util.getArchiveConfig()</em>). Can be overridden if needed.
     * 
     * @return
     */
    protected ArchiveDocConfig getDocumentConfig() {

        return Util.getArchiveConfig()
                   .findArchiveDocConfig(getModule(), getDocument())
                   .get();
    }

    /**
     * NB Not a static class, accesses state from outer class (filter, sort)
     */
    private class LuceneResultsIterable implements AutoClosingIterable<Bean> {

        private final Logger lriLogger = LogManager.getLogger();

        private int readNextRowIdx = 0;
        private final ScoreDoc[] scoreDocs;
        private final DirectoryReader dirReader;
        private final Directory directory;
        private TopDocs topDocs;

        public LuceneResultsIterable(int startRow, int endRow) throws IOException {

            // open the index
            Path indexPath = getDocumentConfig().getIndexDirectory();
            lriLogger.debug("Using index at {}", indexPath);
            directory = FSDirectory.open(indexPath);
            dirReader = DirectoryReader.open(directory);

            IndexSearcher isearcher = new IndexSearcher(dirReader);

            // execute query
            lriLogger.debug("Executing filter {}", filter);
            topDocs = isearcher.search(filter.toQuery(), endRow, getSort());

            // set aside scoredocs
            ScoreDoc[] allScoreDocs = topDocs.scoreDocs;
            scoreDocs = ArrayUtils.subarray(allScoreDocs, startRow, endRow);
            lriLogger.debug("Got {} results, retained {}", allScoreDocs.length, scoreDocs.length);
        }

        /**
         * Iterator is not resettable, calling a 2nd time will break.
         * 
         * @return
         */
        @Override
        public Iterator<Bean> iterator() {
            return this.new LuceneResultsIterator();
        }

        @Override
        public void close() {
            // close index & directory
            tryClose(dirReader);
            tryClose(directory);
        }

        private void tryClose(AutoCloseable ac) {

            try {
                ac.close();
            } catch (Exception e) {
                lriLogger.atWarn()
                         .withThrowable(e)
                         .log("Could not close {}", ac);
            }
        }

        public TotalHits totalHits() {
            return topDocs.totalHits;
        }

        /**
         * NB not a static class
         */
        private class LuceneResultsIterator implements Iterator<Bean> {

            @Override
            public boolean hasNext() {
                return readNextRowIdx < scoreDocs.length;
            }

            @Override
            public Bean next() {
                int docId = scoreDocs[readNextRowIdx].doc;
                ++readNextRowIdx;

                try {
                    Document doc = dirReader.storedFields()
                                            .document(docId);
                    return convertToBean(doc);
                } catch (IOException ioe) {
                    throw new RuntimeException("Unable to retrieve doc #" + docId, ioe);
                }
            }
        }

    }
}
