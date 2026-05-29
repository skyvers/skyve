package org.skyve.impl.archive.list;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.SortedMap;
import java.util.UUID;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.search.FieldExistsQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.TotalHits;
import org.apache.lucene.store.Directory;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.archive.support.ArchiveLuceneIndexerSingleton;
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
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import com.google.common.base.Stopwatch;

/**
 * Abstract Lucene-backed list model for archived Skyve documents.
 * <p>
 * Provides common archived-query behavior (paging, filtering, sorting, summary,
 * and dynamic-bean conversion hooks) while delegating document-specific mapping
 * details to subclasses.
 * </p>
 *
 * @param <U> The bean type.
 */
public abstract class ArchivedDocumentListModel<U extends Bean> extends ListModel<U> {
	// NB An instance member LOGGER is OK here as this is not Serializable
    private final Logger LOGGER = SkyveLoggerFactory.getLogger(getClass());

    private LuceneFilter filter = new LuceneFilter();

    protected org.skyve.metadata.model.document.Document drivingDocument;
    
    private static final ArchiveLuceneIndexerSingleton archiveLuceneIndexerSingleton = ArchiveLuceneIndexerSingleton.getInstance();

    @Override
    public void postConstruct(Customer customer, boolean runtime) {

        drivingDocument = customer.getModule(getModule())
                                  .getDocument(customer, getDocument());
    }

    @Override
    public Filter getFilter() {

        return filter;
    }

    @Override
    public Filter newFilter() {
        return new LuceneFilter();
    }

    @Override
    public void putParameter(String name, Object value) {

        // unused?
    }

    @Override
    public Page fetch() throws Exception {

        LOGGER.debug("Executing fetch, filter={}, start={}, end={}", filter, getStartRow(), getEndRow());

        if (findArchiveDocumentConfig().isEmpty()) {
            LOGGER.debug("No archive config for {}.{} returning an empty page", getModule(), getDocument());
            return emptyPage();
        }

        try {
            Result queryResults = executeQuery();

            Page p = new Page();
            p.setTotalRows(queryResults.totalRowCount());
            p.setRows(queryResults.rows());
            p.setSummary(createSummary(queryResults.totalRowCount()));

            return p;
        } catch (IndexNotFoundException e) {
            LOGGER.atWarn()
                  .setCause(e)
                  .log("No index found, returning empty Page");

            Page p = emptyPage();
            return p;
        }
    }

    private Page emptyPage() {
        Page p = new Page();
        p.setRows(new ArrayList<>());
        p.setSummary(createSummary(0));
        return p;
    }

    private DynamicBean createSummary(long rowCount) {
        HashMap<String, Object> props = new HashMap<>();
        props.put(Bean.DOCUMENT_ID, UUID.randomUUID().toString());
        if (getSummary() != null) {
            if (AggregateFunction.Count == getSummary()) {

                for (MetaDataQueryColumn column : getColumns()) {
                    String binding = column.getBinding();

                    props.put(binding, rowCount);
                }
            } else {
                // We probably can't support the other aggregations types
                LOGGER.warn("Aggregate function {} not supported by {}", getSummary(), this);
            }
        }

        return new DynamicBean(getModule(), getDocument(), props);
    }

    private Result executeQuery() throws IOException {

        Stopwatch t = Stopwatch.createStarted();

        List<Bean> rows = new ArrayList<>(getEndRow() - getStartRow());

        try (LuceneResultsIterable lri = this.new LuceneResultsIterable(getStartRow(), getEndRow())) {

            TotalHits hits = lri.totalHits();

            lri.iterator()
               .forEachRemaining(rows::add);

            LOGGER.debug("Got {} results of {}; took {}", rows.size(), lri.totalHits(), t);

            return new Result(rows, hits.value);
        }
    }

    protected Sort getSort() {

        SortParameter[] params = getSortParameters();
        if (params == null || params.length == 0) {
            Sort defaultSort = getDefaultSort();
            LOGGER.debug("No sorting defined, using {}", defaultSort);
            return defaultSort;
        }

        LOGGER.atDebug()
              .addArgument(() -> Arrays.asList(params))
              .log("SortParameters: {}");

        List<SortField> sortFields = new ArrayList<>(params.length);

        for (SortParameter sp : params) {

            boolean reverse = sp.getDirection() == SortDirection.descending;
            String binding = toSortBinding(sp.getBy());
            SortField sf = new SortField(binding, Type.STRING, reverse);

            LOGGER.debug("Sorting by {}", sf);

            sortFields.add(sf);
        }

        return new Sort(sortFields.toArray(new SortField[0]));
    }

    /**
     * Returns the default Lucene sort when no explicit sort parameters are supplied.
     *
     * <p>Defaults to relevance (Lucene score); subclasses may override to enforce
     * deterministic ordering for their document type.
     *
     * @return the default sort strategy, never {@code null}
     */
    protected Sort getDefaultSort() {
        return Sort.RELEVANCE;
    }

    /**
     * Converts a Skyve binding into the Lucene sort-field binding.
     *
     * <p>This typically appends a sort suffix (for example {@code _sort}) matching
     * the field written by the document converter.
     *
     * @param binding the Skyve binding used in list metadata
     * @return the Lucene field name used for sorting
     */
    protected abstract String toSortBinding(String binding);

    /**
     * Converts a Lucene document into the row bean returned by this list model.
     *
     * <p>{@link DynamicBean} is typically suitable when no concrete generated domain
     * class is required.
     *
     * @param luceneDoc the Lucene document read from the archive index
     * @return the converted bean row to include in list results
     */
    protected abstract Bean convertToBean(Document luceneDoc);

    /**
	 * Reads a stored Lucene document by id.
	 * <p>
	 * Exposed as a protected seam to allow deterministic testing of iterator
	 * error handling without relying on brittle Lucene internals.
	 * </p>
	 */
	@SuppressWarnings("static-method")
	protected Document readStoredDocument(DirectoryReader reader, int docId) throws IOException {
		return reader.storedFields().document(docId);
	}

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
        return drivingDocument;
    }

    /**
     * Returns the module name for the driving archived document.
     *
     * <p>Used to resolve both document metadata and archive configuration.
     *
     * @return the module name used by this model
     */
    protected abstract String getModule();

    /**
     * Returns the document name for the driving archived document.
     *
     * <p>Used to resolve both document metadata and archive configuration.
     *
     * @return the document name used by this model
     */
    protected abstract String getDocument();

    /**
     * Resolves the Lucene index directory configured for this model's module and document.
     *
     * @return the configured index directory path
     * @throws NoSuchElementException if no archive configuration exists for the
     *         module and document combination
     */
    protected Path getIndexDirectory() {
        return Util.getArchiveConfig()
                   .findArchiveDocConfig(getModule(), getDocument())
                   .get()
                   .getIndexDirectory();
    }
    
    /**
     * Finds the archive document configuration for this model's module and document.
     *
     * @return the matching archive configuration, or empty when archiving is not
     *         configured for this document type
     */
    protected Optional<ArchiveDocConfig> findArchiveDocumentConfig() {

        return Util.getArchiveConfig()
                   .findArchiveDocConfig(getModule(), getDocument());
    }

    /**
     * NB Not a static class, accesses state from outer class (filter, sort)
     */
    private class LuceneResultsIterable implements AutoClosingIterable<Bean> {
        private final Logger LRI_LOGGER = SkyveLoggerFactory.getLogger(LuceneResultsIterable.class);

        private int readNextRowIdx = 0;
        private final ScoreDoc[] scoreDocs;
        private final DirectoryReader dirReader;
        private final Directory directory;
        private TopDocs topDocs;

        public LuceneResultsIterable(int startRow, int endRow) throws IOException {

            // open the index
            Path indexPath = getIndexDirectory();
            LRI_LOGGER.debug("Using index at {}", indexPath);
            ArchiveDocConfig archiveDocConfig = Util.getArchiveConfig()
					.findArchiveDocConfig(getModule(), getDocument())
					.get();
            directory = archiveLuceneIndexerSingleton
					.getLuceneConfigs()
					.get(archiveDocConfig).indexDirectory();
            dirReader = DirectoryReader.open(directory);

            IndexSearcher isearcher = new IndexSearcher(dirReader);

            // Execute the query
            Query query;
            if (filter.isEmpty()) {
                // If no criteria have been set search for "bizId is not null" 
                // so that some results are displayed
                query = new FieldExistsQuery(Bean.DOCUMENT_ID);
                LRI_LOGGER.debug("Filter is empty, using default query: {}", query);
            } else {
                // Otherwise run the actual criteria supplied
                query = filter.toQuery();
            }
            LRI_LOGGER.debug("Executing filter {}; query '{}'", filter, query);
            topDocs = isearcher.search(query, endRow, getSort());

            // set aside scoredocs
            ScoreDoc[] allScoreDocs = topDocs.scoreDocs;
            scoreDocs = ArrayUtils.subarray(allScoreDocs, startRow, endRow);
            LRI_LOGGER.debug("Got {} results, retained {}", allScoreDocs.length, scoreDocs.length);
        }

        /**
         * Returns a forward-only iterator over the fetched Lucene score docs.
         *
         * <p>Threading: this iterator is stateful and not resettable; callers must
         * consume it once.
         *
         * @return a stateful iterator over converted bean rows
         */
        @Override
        public Iterator<Bean> iterator() {
            return this.new LuceneResultsIterator();
        }

        @Override
        public void close() {
            // close index & directory
            tryClose(dirReader);
        }

        private void tryClose(AutoCloseable ac) {

            try {
                ac.close();
            } catch (Exception e) {
                LRI_LOGGER.atWarn()
                          .setCause(e)
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
                    Document doc = readStoredDocument(dirReader, docId);
                    return convertToBean(doc);
                } catch (IOException ioe) {
                    throw new RuntimeException("Unable to retrieve doc #" + docId, ioe);
                }
            }
        }
    }
}
