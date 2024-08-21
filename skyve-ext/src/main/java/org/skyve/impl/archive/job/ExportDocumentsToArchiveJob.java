package org.skyve.impl.archive.job;

import static java.time.Instant.now;
import static org.apache.commons.lang3.StringUtils.toRootLowerCase;
import static org.skyve.impl.archive.support.ArchiveUtils.ARCHIVE_CHARSET;
import static org.skyve.impl.archive.support.ArchiveUtils.ARCHIVE_FILE_SUFFIX;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.skyve.CORE;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.archive.support.FileLockRepo;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.persistence.Persistence;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import com.google.common.base.MoreObjects;

import jakarta.inject.Inject;

public class ExportDocumentsToArchiveJob extends CancellableJob {

    private static final char LF = '\n';
    private static final int LOCK_FAILURE = -999;

    @Inject
    private transient Persistence persistence;

    private transient FileLockRepo repo = FileLockRepo.getInstance();

    private final Logger logger = LogManager.getLogger();
    private final Instant targetEndTime;
    private final int batchSize;

    public ExportDocumentsToArchiveJob() {
        targetEndTime = now().plus(Duration.ofSeconds(Util.getArchiveConfig()
                                                          .exportRuntimeSec()));
        batchSize = Util.getArchiveConfig()
                        .exportBatchSize();
    }

    @Override
    public void execute() throws Exception {
        logger.debug("Starting {} with config {}", this, Util.getArchiveConfig());

        if (batchSize <= 0) {
            getLog().add("Invalid batch size configured, job ending");
            return;
        }

        String targetTimeStr = DateTimeFormatter.ISO_LOCAL_TIME.withZone(ZoneId.systemDefault())
                                                               .format(targetEndTime);
        getLog().add("Job started; target end time: " + targetTimeStr);
        getLog().add("Using batch size: " + batchSize);

        int recordsArchived = 0;

        List<ArchiveDocConfig> docConfigs = Util.getArchiveConfig()
                                                .docConfigs();
        for (ArchiveDocConfig archiveDocConfig : docConfigs) {

            if (isCancelled()) {
                break;
            }

            if (timesUp()) {
                break;
            }

            Document doc = getDocument(archiveDocConfig.module(), archiveDocConfig.document());
            JobSubstance js = new JobSubstance(doc, archiveDocConfig);

            logger.debug("Running {}", js);
            int archiveCount = js.execute();
            logger.debug("{} archived {} documents", js, archiveCount);

            recordsArchived += archiveCount;
        }

        if (isCancelled()) {
            getLog().add("Job cancelled");
        } else if (timesUp()) {
            getLog().add("Time's up");
        }

        getLog().add("Done; archived " + recordsArchived + " documents");
    }

    /**
     * Has this job run out of time (i.e. have we pass targetEndTime)?
     * 
     * @return
     */
    public boolean timesUp() {
        return now().isAfter(targetEndTime);
    }

    private Document getDocument(String module, String document) {
        Customer customer = CORE.getUser()
                                .getCustomer();

        return customer.getModule(module)
                       .getDocument(customer, document);
    }

    private class JobSubstance {

        private final Document document;
        private final ArchiveDocConfig config;
        private final String deleteStatement;

        public JobSubstance(Document doc, ArchiveDocConfig config) {
            this.document = doc;
            this.config = config;
            this.deleteStatement = createDeleteStatement();
        }

        public int execute() throws IOException, InterruptedException {

            int exportCount = 0;

            // Loop until time is up, or
            // zero records are exported, or
            // the job is cancelled
            for (int batchNum = 0; true; ++batchNum) {
                logger.debug("Exporting batch #{}", batchNum);

                int num = executeOneBatch();

                if (num == LOCK_FAILURE) {
                    // Couldn't get write lock, try again
                } else if (num <= 0) {
                    // No rows written, we're done
                    logger.debug("0 rows archived");
                    break;
                } else {
                    logger.debug("{} rows archived", num);
                    exportCount += num;
                }

                if (isCancelled()) {
                    break;
                }

                if (timesUp()) {
                    break;
                }
            }

            return exportCount;
        }

        /**
         * Avoid logging the same message repeatedly to the Job log. Only
         * add the message if it's not the same as the last message in the
         * log.
         * 
         * @param msg
         */
        private void logOnce(String msg) {

            List<String> log = getLog();
            if (log.isEmpty()) {
                log.add(msg);
                return;
            }

            String lastMsg = log.get(log.size() - 1);
            if (lastMsg == null || !StringUtils.equals(msg, lastMsg)) {
                log.add(msg);
            }
        }

        /**
         * Execute one batch of writing records to file and deleting from the RDBMS
         * 
         * @param batchSize
         * @return the number of records written/delete
         * @throws InterruptedException
         * @throws Exception
         */
        private int executeOneBatch() throws IOException, InterruptedException {

            File file = getArchiveFile();
            logOnce("Exporting documents to " + file.getName());
            logger.trace("Exporting documents to {}", file);

            Lock lock = repo.getLockFor(file)
                            .writeLock();

            if (lock.tryLock(10, TimeUnit.SECONDS)) {
                // Acquire a write lock and write the batch out
                try {
                    return writeBatch(file);
                } finally {
                    lock.unlock();
                }
            }

            // Couldn't get a lock
            logger.warn("Unable to acquire write lock on {}", file);
            return LOCK_FAILURE;
        }

        private int writeBatch(File file) throws IOException {

            List<DynamicBean> documents = getDocumentsToExport(batchSize);
            if (documents.isEmpty()) {
                return 0;
            }

            Customer c = CORE.getCustomer();
            persistence.begin();

            try (BufferedWriter writer = new BufferedWriter(new FileWriter(file, ARCHIVE_CHARSET, true))) {

                for (DynamicBean currDoc : documents) {

                    logger.trace("Archiving {}", currDoc.getBizId());

                    String entry = JSON.marshall(c, currDoc);

                    writer.write(entry);
                    writer.write(LF);

                    deleteDocument(currDoc);
                }
            } catch (IOException e) {
                logger.atFatal()
                      .withThrowable(e)
                      .log("Writing to archive '{}' failed", file);
                throw e;
            }

            persistence.commit(false);
            persistence.evictAllCached();

            return documents.size();
        }

        public void deleteDocument(DynamicBean currDocument) {

            persistence.newSQL(deleteStatement)
                       .putParameter("id", currDocument.getBizId(), false)
                       .execute();
        }

        public String createDeleteStatement() {
            String table = document.getPersistent()
                                   .getPersistentIdentifier();
            String sql = "delete from " + table + " where bizId = :id";
            logger.trace("Using delete stament {}", sql);
            return sql;
        }

        private File getArchiveFile() {
            Path dir = config.getArchiveDirectory();
            dir.toFile()
               .mkdirs();

            DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd")
                                                     .withZone(ZoneId.systemDefault());
            String datePart = dtf.format(now());
            String fileName = archiveFileNamePrefix() + "-" + datePart + ARCHIVE_FILE_SUFFIX;

            return dir.resolve(fileName)
                      .toFile();
        }

        private String archiveFileNamePrefix() {

            return toRootLowerCase(document.getName());
        }

        private List<DynamicBean> getDocumentsToExport(int howMany) {
            return persistence.newDocumentQuery(document)
                              .setMaxResults(howMany)
                              .projectedResults();
        }

        @Override
        public String toString() {
            return MoreObjects.toStringHelper(this)
                              .add("document", document)
                              .add("config", config)
                              .toString();
        }
    }

}
