package modules.admin.Audit.job;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import jakarta.inject.Inject;
import modules.admin.Audit.job.support.FileLockRepo;
import modules.admin.domain.Audit;

public class AuditArchiveJob extends CancellableJob {

    private static final char LF = '\n';
    private static final int LOCK_FAILURE = -999;

    @Inject
    private transient Persistence persistence;

    @Inject
    private transient FileLockRepo repo;

    // TODO make configurable
    private static final int BATCH_SIZE = 100;
    private final Logger logger = LogManager.getLogger();
    private Instant targetEndTime;

    public AuditArchiveJob() {
        targetEndTime = Instant.now()
                               .plus(Duration.ofSeconds(120));
    }

    @Override
    public void execute() throws Exception {

        String targetTimeStr = DateTimeFormatter.ISO_LOCAL_TIME.withZone(ZoneId.systemDefault())
                                                               .format(targetEndTime);
        getLog().add("Job started; target end time=" + targetTimeStr);
        logAuditCount();

        int recordsArchived = 0;
        for (int batchNum = 0; true; ++batchNum) {
            logger.debug("Exporting batch #{}", batchNum);

            int num = executeOneBatch(BATCH_SIZE);

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
    public int executeOneBatch(int batchSize) throws IOException, InterruptedException {

        File file = getFile();
        getLog().add("Writing audit entries to " + file.getName());
        logger.debug("Writing to " + file.getCanonicalPath() + "; current length=" + file.length());

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
        Collection<String> ids = getElementIds(batchSize);
        if (ids.isEmpty()) {
            return 0;
        }

        persistence.begin();

        try (FileWriter fw = new FileWriter(file, true)) {

            Customer c = CORE.getCustomer();
            for (String bizId : ids) {

                Audit element = persistence.retrieve(Audit.MODULE_NAME, Audit.DOCUMENT_NAME, bizId);
                logger.trace("Archiving {}", element.getBizId());

                String entry = JSON.marshall(c, element);
                fw.write(entry);
                fw.write(LF);

                persistence.delete(element);
            }
        } catch (IOException e) {
            logger.atFatal()
                  .withThrowable(e)
                  .log("Writing to audit archive '" + file + "' failed");
            throw e;
        }

        persistence.commit(false);

        return ids.size();
    }

    private File getFile() {
        Path dir = Path.of(Util.getArchiveDirectory());
        dir.toFile()
           .mkdirs();

        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd")
                                                 .withZone(ZoneOffset.UTC);
        String datePart = dtf.format(Instant.now());
        String fileName = "audits-" + datePart;

        return dir.resolve(fileName)
                  .toFile();
    }

    protected Collection<String> getElementIds(int howMany) {
        return persistence.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME)
                          .setMaxResults(howMany)
                          .addBoundProjection(Bean.DOCUMENT_ID)
                          .scalarResults(String.class);
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
