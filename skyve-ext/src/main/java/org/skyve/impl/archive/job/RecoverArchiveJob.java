package org.skyve.impl.archive.job;

import static org.skyve.archive.support.ArchiveableBean.archiveFilenamePropertyName;
import static org.skyve.archive.support.ArchiveableBean.archiveTimestampPropertyName;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock.WriteLock;

import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.TermQuery;
import org.skyve.CORE;
import org.skyve.archive.support.CorruptArchiveError;
import org.skyve.archive.support.CorruptArchiveError.Resolution;
import org.skyve.impl.archive.support.ArchiveLuceneIndexerSingleton;
import org.skyve.impl.archive.support.FileLockRepo;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;

public class RecoverArchiveJob extends CancellableJob {

    private static final String CORRUPT_FILE_SUFFIX = ".corrupt";

    private static final Logger LOGGER = LoggerFactory.getLogger(RecoverArchiveJob.class);
    
    private static final ArchiveLuceneIndexerSingleton archiveLuceneIndexerSingleton = ArchiveLuceneIndexerSingleton.getInstance();

    @Inject
    private Persistence persistence;

    private FileLockRepo repo = FileLockRepo.getInstance();

    @Override
    public void execute() throws Exception {

        List<CorruptArchiveError> unresolved = listUnresolvedErrors();
        getLog().add(String.format("%,d errors to resolve", unresolved.size()));

        for (CorruptArchiveError error : unresolved) {
            try {
                attemptRecovery(error);
            } catch (RecoveryException e) {
                persistence.begin();

                String msg = "Recovery encountered an error";
                LOGGER.atWarn()
                      .setCause(e)
                      .log(msg);
                getLog().add(msg + " " + e);

                error.setResolution(Resolution.failed);
                persistence.save(error);
                persistence.commit(false);
            }

            if (isCancelled()) {
                getLog().add("Recovery process cancelled");
                break;
            }
        }

        getLog().add("Recovery done");
    }

    private void attemptRecovery(CorruptArchiveError error) throws IOException, InterruptedException {

        LOGGER.debug("Attempting recovery of: {}", error.getBizKey());

        // safety check on age of error?
        // hard to do in a headless job...

        ArchiveDocConfig config = findConfig(error);

        String filename = error.getFilename();
        Path archivePath = config.getArchiveDirectory()
                                 .resolve(filename);

        assertFileExists(config, archivePath);

        WriteLock lock = repo.getLockFor(archivePath.toFile())
                             .writeLock();
        for (int i = 10; i > 0; --i) {
            LOGGER.trace("Waiting for write lock on {}", archivePath);
            if (lock.tryLock(1, TimeUnit.MINUTES)) {
                try {
                    getLog().add("Starting recovery on " + filename);
                    LOGGER.trace("Got write lock on {}", archivePath);
                    doRecovery(error, archivePath, config);
                    return;
                } finally {
                    lock.unlock();
                }
            }
        }

        log("Unable to acquire write lock on " + archivePath);
        return;
    }

    private void log(String msg) {
        getLog().add(msg);
        LOGGER.debug(msg);
    }

    /**
     * Now we've got the write lock on the corrupt file, try to do the recorvery process.
     * 
     * @param error
     * @param archivePath
     * @param config
     */
    private void doRecovery(CorruptArchiveError error, Path archivePath, ArchiveDocConfig config) {

        persistence.begin();

        // Update error resolution
        error.setResolution(Resolution.inProgress);
        persistence.save(error);

        // Revert soft-deletes (RDBMS)
        log("Reverting soft-deletes");
        int revertCount = revertSoftDeletes(error);
        log("Revert soft-delete modified " + revertCount + " rows");

        // Delete index entries (lucene)
        log("Deleting lucene index entries");
        deleteIndexReferences(config, error.getFilename());

        // Mark the error resolved
        error.setResolution(Resolution.resolved);
        persistence.save(error);

        // Commit before moving the file; if the file rename fails
        // it will be indexed again, and a new unresolved error created
        persistence.commit(false);

        // Rename the corrupted file
        renameArchive(archivePath);
    }

    /**
     * Try to move the corrupt archive to a new path.
     */
    private void renameArchive(final Path archivePath) {

        Path destination = chooseDestinationPath(archivePath);
        log("Renaming corrupted archive file from " + archivePath + " to " + destination);

        try {
            Files.move(archivePath, destination);
        } catch (IOException e) {
            throw new RecoveryException("Unable to rename " + archivePath + " to " + destination, e);
        }
    }

    /**
     * Given the corrupt archive's path, find a destination to move it to. We'll try
     * appending ".0.corrupt" to the filename; continuing to ".1.corrupt", ".2.corrupt"
     * and so on until ".99.corrupt". At that point blowing up with a RecoveryException.
     * 
     * @param archivePath
     * @return
     */
    private Path chooseDestinationPath(final Path archivePath) {
        for (int i = 0; i < 100; ++i) {

            Path destination = archivePath.resolveSibling(archivePath.getFileName() + "." + i + CORRUPT_FILE_SUFFIX);
            if (Files.notExists(destination)) {
                return destination;
            }
        }

        throw new RecoveryException("Unable to find destination path for corrupt archive: " + archivePath);
    }

    /**
     * Clear the soft delete attributes (ie: archiveFilename & archiveTimestamp)
     * from the relevant table (eg admin.Audit).
     * 
     * @param error
     * @return
     */
    private int revertSoftDeletes(CorruptArchiveError error) {

        Document doc = getDocument(error.getArchiveTypeModule(), error.getArchiveTypeDocument());
        @SuppressWarnings("null")
		String tableName = doc.getPersistent()
                              .getPersistentIdentifier();

        String sql = undoSoftDeleteSQL(tableName);
        LOGGER.trace("Using update statement: {}", sql);

        SQL undoDelete = persistence.newSQL(sql);
        undoDelete.putParameter("filename", error.getFilename(), false);
        return undoDelete.execute();
    }

    private String undoSoftDeleteSQL(String tableName) {

        final String undoSoftDeleteSQL = """
                update %s
                  set  %s = null
                      ,%s = null
                where %s = :filename
                """;

        return String.format(undoSoftDeleteSQL, tableName,
                archiveTimestampPropertyName, archiveFilenamePropertyName, archiveFilenamePropertyName);
    }

    private Document getDocument(String module, String document) {
        Customer customer = CORE.getUser()
                                .getCustomer();

        return customer.getModule(module)
                       .getDocument(customer, document);
    }

    /**
     * Delete references in the lucene index. Both the progress entry, and
     * any document references which refer to the given filename.
     */
    private void deleteIndexReferences(ArchiveDocConfig config, String filename) {

        Query archiveContents = new TermQuery(new Term(IndexArchivesJob.FILENAME_FIELD, filename));
        Query progressContents = new TermQuery(new Term(IndexArchivesJob.PROGRESS_FILENAME_FIELD, filename));

        LOGGER.trace("Deleting index entries with: {}", archiveContents);
        LOGGER.trace("Deleting progress entries with: {}", progressContents);

        try {
        	@SuppressWarnings("resource")
			IndexWriter writer = archiveLuceneIndexerSingleton.getIndexWriter(config);
            writer.deleteDocuments(archiveContents, progressContents);
        } catch (

        IOException e) {
            throw new RecoveryException("Encountered an error interacting with index", e);
        }
    }

    /**
     * Blow up if the .archive file doesn't exist, or isn't file.
     */
    private void assertFileExists(ArchiveDocConfig config, Path archivePath) {
        if (!Files.isRegularFile(archivePath)) {

            String msg = String.format("Corrupt archive path '%s' is invalid, unable to continue; doc config: %s",
                    archivePath, config);

            getLog().add(msg);
            LOGGER.warn(msg);
            throw new RecoveryException(msg);
        }
    }

    private ArchiveDocConfig findConfig(CorruptArchiveError error) {

        String module = error.getArchiveTypeModule();
        String document = error.getArchiveTypeDocument();
        return Util.getArchiveConfig()
                   .findArchiveDocConfig(module, document)
                   .orElseThrow(() -> new RecoveryException("Unable to find archive config for " + module + "." + document));
    }

    private List<CorruptArchiveError> listUnresolvedErrors() {
        DocumentQuery errorQuery = persistence.newDocumentQuery(CorruptArchiveError.MODULE_NAME, CorruptArchiveError.DOCUMENT_NAME);
        errorQuery.getFilter()
                  .addEquals(CorruptArchiveError.resolutionPropertyName, Resolution.unresolved);

        return errorQuery.beanResults();
    }

    private static class RecoveryException extends RuntimeException {

        public RecoveryException(String msg) {
            super(msg);
        }

        public RecoveryException(String msg, Throwable cause) {
            super(msg, cause);
        }
    }
}
