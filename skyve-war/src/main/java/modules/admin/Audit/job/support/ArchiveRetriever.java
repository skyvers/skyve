package modules.admin.Audit.job.support;

import static java.util.Collections.emptyList;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

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
import modules.admin.Audit.job.ArchiveIndexJob;
import modules.admin.Audit.models.LuceneFilter;
import modules.admin.domain.Audit;

@Singleton
public class ArchiveRetriever {

    private final Logger logger = LogManager.getLogger();

    private static final String READ_ONLY = "r";

    /**
     * Retrieve 0 or 1 Audit entry via the <em>Audit</em>'s bizId.
     * 
     * @param bizId
     * @return
     */
    public Audit retrieveByBizId(String bizId) {

        try {
            LuceneFilter lf = new LuceneFilter();
            lf.addEquals(Bean.DOCUMENT_ID, bizId);

            List<ArchiveEntry> entries = searchIndex(lf, 1);
            if (entries.isEmpty()) {
                return null;
            }
            ArchiveEntry entry = entries.getFirst();
            return retrieveBean(entry);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Retrive all
     * 
     * @param auditBizId
     * @return
     */
    public List<Audit> retrieveByAuditBizId(String auditBizId) {

        LuceneFilter filter = new LuceneFilter();
        filter.addEquals(Audit.auditBizIdPropertyName, auditBizId);

        try {
            List<ArchiveEntry> entries = searchIndex(filter, 100);

            List<Audit> audits = new ArrayList<>(entries.size());
            for (ArchiveEntry entry : entries) {
                Audit a = retrieveBean(entry);
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

        Path auditArchiveIndexPath = ArchiveIndexJob.getIndexPath();
        logger.debug("Searching for {}; using index at {}", filter, auditArchiveIndexPath);

        try (Directory directory = FSDirectory.open(auditArchiveIndexPath);
                DirectoryReader ireader = DirectoryReader.open(directory)) {

            IndexSearcher isearcher = new IndexSearcher(ireader);
            TopDocs td = isearcher.search(filter.toQuery(), maxResults);

            if (td.scoreDocs.length < 1) {
                logger.warn("No index entries found for {}", filter);
                return emptyList();
            }

            List<ArchiveEntry> entries = new ArrayList<>();

            for (ScoreDoc sd : td.scoreDocs) {

                int docID = sd.doc;
                Document doc = ireader.storedFields()
                                      .document(docID);

                long offset = doc.getField(ArchiveIndexJob.OFFSET_FIELD)
                                 .numericValue()
                                 .longValue();
                String fileName = doc.get(ArchiveIndexJob.FILENAME_FIELD);

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
