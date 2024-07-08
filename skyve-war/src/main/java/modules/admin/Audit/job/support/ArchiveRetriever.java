package modules.admin.Audit.job.support;

import static java.util.Collections.emptyList;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import jakarta.inject.Singleton;
import modules.admin.Audit.job.IndexArchivesJob;
import modules.admin.Audit.models.LuceneFilter;

/**
 * Utility class for retrieving Beans archived to file.
 */
@Singleton
public class ArchiveRetriever {

    private final Logger logger = LogManager.getLogger();

    private static final String READ_ONLY = "r";

    /**
     * Retrieve 0 or 1 archived bean entry via that <em>Bean</em>'s bizId. If the bean has
     * been archived more than once which instance is returned is not defined (though
     * they should be identical). Requires the Bean's bizId to have been stored in a
     * field called "bizId".
     * 
     * @param bizId bizId of the archived bean to retrieve
     * @return an Optional containing the requested Bean, or an empty optional
     *         if it could not be found.
     */
    public <T extends Bean> Optional<T> retrieveByBizId(String bizId) {

        try {
            LuceneFilter lf = new LuceneFilter();
            lf.addEquals(Bean.DOCUMENT_ID, bizId);

            List<ArchiveEntry> entries = searchIndex(lf, 1);
            if (entries.isEmpty()) {
                return Optional.empty();
            }

            ArchiveEntry entry = entries.get(0);
            return Optional.ofNullable(retrieveBean(entry));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Retrive all archived beans that match the supplied <em>LuceneFilter</em>.
     * 
     * @param filter Filter to use to search the index
     * @return a list of results (empty if none are found)
     */
    public <T extends Bean> List<T> retrieveAll(LuceneFilter filter, int maxResults) {

        try {
            List<ArchiveEntry> entries = searchIndex(filter, maxResults);

            List<T> audits = new ArrayList<>(entries.size());
            for (ArchiveEntry entry : entries) {
                T a = retrieveBean(entry);
                audits.add(a);
            }

            return audits;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    /**
     * Search the audit archive index using the provided filter, returning the file
     * and offset where each result record can be found; or and empty list if nothing
     * is found.
     * 
     * @param bizId
     * @return
     * @throws IOException
     */
    private List<ArchiveEntry> searchIndex(LuceneFilter filter, int maxResults) throws IOException {

        Path auditArchiveIndexPath = IndexArchivesJob.getIndexPath();
        logger.debug("Searching for {}; using index at {}", filter, auditArchiveIndexPath);

        try (Directory directory = FSDirectory.open(auditArchiveIndexPath);
                DirectoryReader ireader = DirectoryReader.open(directory)) {

            IndexSearcher isearcher = new IndexSearcher(ireader);
            TopDocs td = isearcher.search(filter.toQuery(), maxResults);

            if (td.scoreDocs.length < 1) {
                logger.debug("No index entries found for {}", filter);
                return emptyList();
            }

            List<ArchiveEntry> entries = new ArrayList<>();

            for (ScoreDoc sd : td.scoreDocs) {

                int docID = sd.doc;
                Document doc = ireader.storedFields()
                                      .document(docID);

                long offset = doc.getField(IndexArchivesJob.OFFSET_FIELD)
                                 .numericValue()
                                 .longValue();
                String fileName = doc.get(IndexArchivesJob.FILENAME_FIELD);

                ArchiveEntry entry = new ArchiveEntry(fileName, offset);
                entries.add(entry);
            }

            logger.debug("Found {} results for {}", entries.size(), filter);
            return entries;
        }
    }

    @SuppressWarnings("unchecked")
    private <T extends Bean> T retrieveBean(ArchiveEntry entry) {

        try {
            String line = readLine(getArchiveFilePath(entry.fileName()), entry.offset());
            return (T) JSON.unmarshall(CORE.getUser(), line);
        } catch (Exception e) {
            logger.atFatal()
                  .withThrowable(e)
                  .log("Unable to retrieve archived entry {}", entry);
            throw new RuntimeException(e);
        }
    }

    private String readLine(Path filePath, long offset) throws IOException {

        try (RandomAccessFile raf = new RandomAccessFile(filePath.toFile(), READ_ONLY)) {
            raf.seek(offset);
            return raf.readLine();
        }
    }

    private Path getArchiveFilePath(String fileName) {
        Path p = Path.of(Util.getArchiveDirectory(), fileName);

        if (Files.isRegularFile(p)) {
            return p;
        }

        throw new IllegalArgumentException("Archive file path '" + fileName + "' is not valid");
    }

    private record ArchiveEntry(String fileName, long offset) {
    }
}
