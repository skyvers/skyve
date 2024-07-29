package modules.admin.Audit.job;

import static java.util.stream.Collectors.toList;
import static modules.admin.Audit.job.support.ArchiveUtils.excerptLine;
import static modules.admin.Audit.job.support.ArchiveUtils.getIndexPath;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
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
import org.skyve.domain.Bean;
import org.skyve.job.CancellableJob;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import jakarta.inject.Inject;
import modules.admin.Audit.job.support.AuditDocumentConverter;
import modules.admin.Audit.job.support.BufferedLineReader;
import modules.admin.Audit.job.support.BufferedLineReader.Line;
import modules.admin.Audit.job.support.FileLockRepo;
import modules.admin.domain.Audit;

public class IndexArchivesJob extends CancellableJob {


    private static final Logger logger = LogManager.getLogger();

    // Fields added to each Audit document index entry
    public static String FILENAME_FIELD = "_filename";
    public static String OFFSET_FIELD = "_offset";

    // Fields used to record indexing progress of each file
    private static String PROGRESS_FILENAME_FIELD = "_progress_filename";
    private static String PROGRESS_OFFSET_FIELD = "_progress_offset";

    private AuditDocumentConverter converter = new AuditDocumentConverter();

    @Inject
    private FileLockRepo repo;

    @Override
    public void execute() throws Exception {

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

        if (isCancelled()) {
            getLog().add("Job cancelled");
        } else {
            getLog().add("Job done");
        }
    }

    private void processFile(IndexableFile indexableFile) throws IOException {

        File file = indexableFile.file();
        long startOffset = indexableFile.startOffset();
        String msg = String.format("Indexing %s starting at %d", file.getName(), startOffset);
        logger.debug(msg);
        getLog().add(msg);

        try (Analyzer analyzer = newAnalyzer();
                Directory directory = FSDirectory.open(getIndexPath())) {
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

        // Convert the Audit document and add to index
        // TODO add some flexibility here with multiple converters
        Document doc = converter.convert((Audit) entryBean);
        doc.add(new StoredField(FILENAME_FIELD, fileName));
        doc.add(new StoredField(OFFSET_FIELD, offset));
        iwriter.addDocument(doc);

        // Record progress through the file
        updateProgress(fileName, lineRecord.end(), iwriter);
    }

    /**
     * Unmarshall
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

        List<File> archives = listArchiveFiles();
        logger.debug("{} archive files found in {}", archives.size(), getIndexPath());

        List<IndexableFile> unindexed = new ArrayList<>();

        try (Directory directory = FSDirectory.open(getIndexPath());
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
            logger.info("No index found at {}", getIndexPath());
            return archives.stream()
                           .map(f -> new IndexableFile(f, 0))
                           .collect(toList());
        }

        return unindexed;
    }

    /**
     * List the .archive files in the configured archive directory.
     * 
     * @return
     * @throws IOException
     */
    private List<File> listArchiveFiles() throws IOException {
        Path dir = Util.getArchiveDirectory();

        try (Stream<Path> s = Files.list(dir)) {
            return s.map(Path::toFile)
                    .filter(File::isFile)
                    .filter(f -> f.getName()
                                  .endsWith(ExportAuditsToArchiveJob.ARCHIVE_FILE_SUFFIX))
                    .collect(toList());
        }
    }

    private static record IndexableFile(File file, long startOffset) {
    }

}
