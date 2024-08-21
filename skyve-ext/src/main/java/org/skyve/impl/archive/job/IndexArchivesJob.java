package org.skyve.impl.archive.job;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.skyve.impl.archive.support.ArchiveUtils.ARCHIVE_FILE_SUFFIX;
import static org.skyve.impl.archive.support.ArchiveUtils.excerptLine;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock.ReadLock;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.core.KeywordTokenizerFactory;
import org.apache.lucene.analysis.core.LowerCaseFilterFactory;
import org.apache.lucene.analysis.custom.CustomAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.LongField;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexableField;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.skyve.CORE;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.support.BufferedLineReader;
import org.skyve.impl.archive.support.BufferedLineReader.Line;
import org.skyve.impl.archive.support.FileLockRepo;
import org.skyve.impl.util.UtilImpl.ArchiveConfig;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.job.CancellableJob;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import jakarta.enterprise.inject.Any;
import jakarta.enterprise.inject.Instance;
import jakarta.inject.Inject;

public class IndexArchivesJob extends CancellableJob {

    private static final Logger logger = LogManager.getLogger();

    // Fields added to each archived document index entry
    public static String FILENAME_FIELD = "_filename";
    public static String OFFSET_FIELD = "_offset";
    public static String LENGTH_FIELD = "_length";
    public static String DOC_TYPE_FIELD = "_doctype";

    // Fields used to record indexing progress of each file
    private static String PROGRESS_FILENAME_FIELD = "_progress_filename";
    private static String PROGRESS_OFFSET_FIELD = "_progress_offset";

    private FileLockRepo repo = FileLockRepo.getInstance();

    @Inject
    @Any
    private Instance<DocumentConverter> documentConverters;

    @Override
    public void execute() throws Exception {

        ArchiveConfig config = Util.getArchiveConfig();
        logger.trace("Starting {} with config {}", this, config);

        if (config.exportBatchSize() <= 0) {
            getLog().add("Invalid batch size configured, job ending");
            return;
        }

        for (ArchiveDocConfig docConfig : config.docConfigs()) {

            if (isCancelled()) {
                break;
            }

            getLog().add("Indexing " + docConfig);

            Path archiveDir = docConfig.getArchiveDirectory();
            Path indexDir = docConfig.getIndexDirectory();

            // Lookup the converter to use
            Optional<DocumentConverter> converter = lookupConverter(docConfig.module(), docConfig.document());
            if (converter.isEmpty()) {
                getLog().add("No suitable document converter available for " + docConfig + "; skipping");
                continue;
            }

            IndexDocumentsProcess process = new IndexDocumentsProcess(archiveDir, indexDir, converter.get());
            process.execute();
        }

        if (isCancelled()) {
            getLog().add("Job cancelled");
        } else {
            getLog().add("Job done");
        }
    }

    /**
     * Find (via CDI) a DocumentConverter instance that can handle the requested module+document (as determined
     * by calling <em>DocumentConverter.handles(String, String)</em> on available instances.
     * <p>
     * If more than one DocumentConverter instance can handle the document type the particular instance 
     * which is returned is undefined.
     * 
     * @return An optional containing a suitable DocumentConverter, or an empty option if no suitable converter is found.
     */
    public Optional<DocumentConverter> lookupConverter(String module, String document) {

        Optional<DocumentConverter> converter = documentConverters.stream()
                                                                  .filter(dc -> dc.handles(module, document))
                                                                  .findFirst();

        if (converter.isPresent()) {
            logger.debug("Using converter: {} for {}.{}", converter, module, document);
        } else {
            logger.warn("No suitable document converter found for {}.{}; available converters: {}", module, document,
                    documentConverters.stream()
                                      .map(Object::toString)
                                      .collect(joining(", ")));
        }

        return converter;
    }

    private class IndexDocumentsProcess {

        /**
         * Directory containing the .archive files
         */
        private final Path archiveDir;

        /**
         * Directory containing the lucene index
         */
        private final Path indexDir;

        /**
         * Document type this instance will be indexing
         */
        private final DocumentConverter converter;

        public IndexDocumentsProcess(Path archiveDir, Path indexDir, DocumentConverter converter) {
            this.archiveDir = archiveDir;
            this.indexDir = indexDir;
            this.converter = converter;
        }

        public void execute() throws IOException, ParseException, InterruptedException {

            logger.debug("Indexing files in {}", archiveDir);
            logger.debug("Using lucene index {}", indexDir);

            // Identify which files have unindexed changes
            List<IndexableFile> unindexed = identifyUnindexed();

            getLog().add(unindexed.size() + " files to index");
            unindexed.forEach(un -> getLog().add(un.file.getName() + " [" + un.startOffset + "]"));

            // Index (parts) of those files one at a time
            for (IndexableFile indexableFile : unindexed) {

                if (isCancelled()) {
                    break;
                }

                ReadLock lock = repo.getLockFor(indexableFile.file())
                                    .readLock();
                logger.debug("Locking {} for reading", indexableFile.file());
                if (lock.tryLock(1, TimeUnit.MINUTES)) {
                    try {
                        processFile(indexableFile);
                    } finally {
                        logger.debug("Releasing lock on {}", indexableFile.file());
                        lock.unlock();
                    }
                } else {
                    logger.warn("Could not acquire read lock on {}", indexableFile.file());
                }
            }

        }

        private void processFile(IndexableFile indexableFile) throws IOException {

            File file = indexableFile.file();
            long startOffset = indexableFile.startOffset();
            String msg = String.format("Indexing %s starting at %d", file.getName(), startOffset);
            logger.debug(msg);
            getLog().add(msg);

            try (Analyzer analyzer = newAnalyzer();
                    Directory directory = FSDirectory.open(indexDir)) {
                IndexWriterConfig config = new IndexWriterConfig(analyzer);
                try (IndexWriter iwriter = new IndexWriter(directory, config)) {

                    // Read every line out starting from the given offset
                    // or until the job is cancelled
                    try (BufferedLineReader blr = new BufferedLineReader(file.toPath(), startOffset)) {

                        for (Line line = blr.readLine(); line != null && !isCancelled(); line = blr.readLine()) {

                            try {
                                indexLine(file.getName(), line, iwriter);
                            } catch (Exception e) {
                                String errMsg = String.format("Error indexing record at byte offset %d in %s; line excerpt: '%s'",
                                        line.offset(), file.getName(), excerptLine(line.line()));
                                logger.atFatal()
                                      .withThrowable(e)
                                      .log(errMsg);
                                getLog().add(errMsg);
                                // Give up indexing this file, but we will attempt to continue
                                // indexing any others
                                break;
                            }

                        }
                    }
                }
            }
        }

        /**
         * Create a custom analyzer. Uses the KeywordTokenizer and a LowercaseFilter.
         * 
         * @return
         */
        private Analyzer newAnalyzer() {

            try {
                return CustomAnalyzer.builder()
                                     .addTokenFilter(LowerCaseFilterFactory.NAME)
                                     .withTokenizer(KeywordTokenizerFactory.NAME)
                                     .build();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

        private void indexLine(String fileName, Line lineRecord, IndexWriter iwriter) throws IOException {

            long offset = lineRecord.offset();

            logger.trace("Indexing line in {} at {}-{}", fileName, offset, lineRecord.end());
            String line = lineRecord.line()
                                    .trim();

            Bean entryBean = unmarshallBean(line);

            // Doctype may not be necessary...
            String docType = entryBean.getBizModule() + "." + entryBean.getBizDocument();

            // Convert the document and add to index
            Document doc = converter.convert(entryBean);

            // Standard entry metadata for all documents
            doc.add(new StoredField(FILENAME_FIELD, fileName));
            doc.add(new StoredField(OFFSET_FIELD, offset));
            doc.add(new StoredField(LENGTH_FIELD, lineRecord.length()));
            doc.add(new StringField(DOC_TYPE_FIELD, docType, Store.YES));
            iwriter.addDocument(doc);

            // Record progress through the file
            updateProgress(fileName, lineRecord.end(), iwriter);
        }

        /**
         * Unmarshall a bean from the provided string
         * 
         * @param fileName
         * @param offset
         * @param line
         * @return
         */
        public Bean unmarshallBean(String line) {
            try {
                Bean bean = (Bean) JSON.unmarshall(CORE.getUser(), line);
                logger.trace("Unmarshalled {}", bean);
                return bean;
            } catch (Exception e) {
                logger.atFatal()
                      .withThrowable(e)
                      .log("Could not unmarshall archive entry: {}", line);
                throw new RuntimeException("Could not unmarshall record", e);
            }
        }

        private void updateProgress(String fileName, Long offset, IndexWriter iwriter) throws IOException {

            Document progressDoc = new Document();
            progressDoc.add(new StringField(PROGRESS_FILENAME_FIELD, fileName, Store.YES));
            progressDoc.add(new LongField(PROGRESS_OFFSET_FIELD, offset, Store.YES));

            iwriter.updateDocument(new Term(PROGRESS_FILENAME_FIELD, fileName), progressDoc);
            logger.trace("Marking file progress; file={}, offset={}", fileName, offset);
        }

        /**
         * Search the archive directory for files (no-recursion); if the file is longer tha
         * 
         * @return
         * @throws IOException
         * @throws ParseException
         */
        private List<IndexableFile> identifyUnindexed() throws IOException, ParseException {

            List<File> archives = listArchiveFiles(archiveDir);
            logger.debug("{} archive files found in {}", archives.size(), archiveDir);

            List<IndexableFile> unindexed = new ArrayList<>();

            try (Directory directory = FSDirectory.open(indexDir);
                    DirectoryReader ireader = DirectoryReader.open(directory);
                    Analyzer analyzer = newAnalyzer()) {
                IndexSearcher isearcher = new IndexSearcher(ireader);

                QueryParser qp = new QueryParser(PROGRESS_FILENAME_FIELD, analyzer);

                for (File file : archives) {

                    logger.trace("Looking for unindexed changes to {}", file);

                    // Query the index
                    // We're going to use just the file name (rather than path)
                    // to identify files, so the archive & index can be moved
                    // if needed
                    Query query = qp.parse(file.getName());
                    TopDocs td = isearcher.search(query, 1);

                    if (td.scoreDocs.length == 0) {
                        logger.trace("No index entry for {}", file);
                        unindexed.add(new IndexableFile(file, 0));
                        continue;
                    }

                    Document doc = ireader.storedFields()
                                          .document(td.scoreDocs[0].doc);
                    // compare size to expected size
                    final long actualSize = file.length();
                    IndexableField offsetField = doc.getField(PROGRESS_OFFSET_FIELD);
                    Number offsetValue = offsetField.numericValue();
                    if (offsetValue.longValue() == actualSize) {
                        // No changes since last time
                        logger.trace("No changes to index for {} [{}]", file, actualSize);
                        continue;
                    }

                    logger.trace("Unindexed changes in {}, indexed offset: {}; actual size: {}",
                            file, offsetValue, actualSize);

                    // add changes to unindexed
                    unindexed.add(new IndexableFile(file, offsetValue.longValue()));
                }
            } catch (@SuppressWarnings("unused") IndexNotFoundException infe) {
                // If the index doesn't exist we'll need to index every file starting
                // from byte zero.
                logger.info("No index found at {}", indexDir);
                return archives.stream()
                               .map(f -> new IndexableFile(f, 0))
                               .collect(toList());
            }

            return unindexed;
        }
    }

    /**
     * List the .archive files in the given archive directory.
     * 
     * @return
     * @throws IOException
     */
    private static List<File> listArchiveFiles(Path dir) throws IOException {

        try (Stream<Path> s = Files.list(dir)) {
            return s.map(Path::toFile)
                    .filter(File::isFile)
                    .filter(f -> f.getName()
                                  .endsWith(ARCHIVE_FILE_SUFFIX))
                    .collect(toList());
        }
    }

    /**
     * Tuple record for tracking where to start indexing .archive files
     */
    private static record IndexableFile(File file, long startOffset) {
    }

}
