package modules.admin.Audit.job;

import static modules.admin.Audit.job.support.ArchiveUtils.ARCHIVE_CHARSET;

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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import jakarta.inject.Inject;
import modules.admin.Audit.job.support.FileLockRepo;
import modules.admin.domain.Audit;

public class ExportAuditsToArchiveJob extends CancellableJob {

    private static final char LF = '\n';
    private static final int LOCK_FAILURE = -999;

    @Inject
    private transient Persistence persistence;

    @Inject
    private transient FileLockRepo repo;

    public static final String ARCHIVE_FILE_SUFFIX = ".archive";

    private final Logger logger = LogManager.getLogger();
    private Instant targetEndTime;

    public ExportAuditsToArchiveJob() {
        targetEndTime = Instant.now()
                               .plus(Duration.ofSeconds(UtilImpl.ARCHIVE_EXPORT_RUNTIME_SEC));
    }

    @Override
    public void execute() throws Exception {

        String targetTimeStr = DateTimeFormatter.ISO_LOCAL_TIME.withZone(ZoneId.systemDefault())
                                                               .format(targetEndTime);
        getLog().add("Job started; target end time=" + targetTimeStr);
        getLog().add("Using batch size: " + UtilImpl.ARCHIVE_EXPORT_BATCH_SIZE);
        logAuditCount();

        int recordsArchived = 0;
        for (int batchNum = 0; true; ++batchNum) {
            logger.debug("Exporting batch #{}", batchNum);

            int num = executeOneBatch(UtilImpl.ARCHIVE_EXPORT_BATCH_SIZE);

            if (num == LOCK_FAILURE) {
                // Couldn't get write lock, try again
            } else if (num <= 0) {
                // No rows written, we're done
                logger.debug("0 rows archived");
                break;
            } else {
                logger.debug("{} rows archived", num);
                recordsArchived += num;
            }

            if (isCancelled()) {
                getLog().add("Job cancelled");
                break;
            }

            if (targetEndTime.isBefore(Instant.now())) {
                getLog().add("Time's up");
                break;
            }
        }

        getLog().add("Done; archived " + recordsArchived + " rows");
        logAuditCount();
    }

    /**
     * Execute one batch of writing records to file and deleting from the RDBMS
     * 
     * @param batchSize
     * @return the number of records written/delete
     * @throws InterruptedException
     * @throws Exception
     */
    private int executeOneBatch(int batchSize) throws IOException, InterruptedException {

        File file = getFile();
        getLog().add("Writing audit entries to " + file.getName());

        Lock lock = repo.getLockFor(file)
                        .writeLock();

        if (lock.tryLock(10, TimeUnit.SECONDS)) {
            // Acquire a write lock and write the batch out
            try {
                return writeBatch(batchSize, file);
            } finally {
                lock.unlock();
            }
        }

        // Couldn't get a lock
        logger.warn("Unable to acquire write lock on {}", file);
        return LOCK_FAILURE;
    }

    private int writeBatch(int batchSize, File file) throws IOException {

        List<DynamicBean> audits = getElements(batchSize);

        if (audits.isEmpty()) {
            return 0;
        }

        Customer c = CORE.getCustomer();
        persistence.begin();

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(file, ARCHIVE_CHARSET, true))) {

            for (DynamicBean currAudit : audits) {

                logger.trace("Archiving {}", currAudit.getBizId());

                String entry = JSON.marshall(c, currAudit);

                writer.write(entry);
                writer.write(LF);

                persistence.newSQL("delete from ADM_Audit where bizId = :id")
                           .putParameter("id", currAudit.getBizId(), false)
                           .execute();
            }
        } catch (IOException e) {
            logger.atFatal()
                  .withThrowable(e)
                  .log("Writing to audit archive '" + file + "' failed");
            throw e;
        }

        persistence.commit(false);
        persistence.evictAllCached();

        return audits.size();
    }

    private File getFile() {
        Path dir = Util.getArchiveDirectory();
        dir.toFile()
           .mkdirs();

        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd")
                                                 .withZone(ZoneId.systemDefault());
        String datePart = dtf.format(Instant.now());
        String fileName = "audits-" + datePart + ARCHIVE_FILE_SUFFIX;

        return dir.resolve(fileName)
                  .toFile();
    }

    protected List<DynamicBean> getElements(int howMany) {
        return persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME)
                          .setMaxResults(howMany)
                          .projectedResults();
    }

    private void logAuditCount() {
        Long count = countAuditEntries();
        String msg = String.format("%,d audit logs", count);
        getLog().add(msg);
        logger.debug(msg);
    }

    private Long countAuditEntries() {
        return persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME)
                          .addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "count")
                          .scalarResult(Long.class);
    }

}
