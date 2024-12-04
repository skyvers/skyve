package org.skyve.impl.archive.job;

import java.util.List;
import java.util.Optional;

import org.skyve.job.CancellableJob;

/**
 * Run the three component jobs of the archival process.
 * 
 * Recover: look for any errors recorded in the database, and rectify them
 * Index: index any new archive changes, recording errors if found
 * Export: convert skyve documents from the RDBMS to the filesystem
 */
public class ArchiveJob extends CancellableJob {

    private Optional<CancellableJob> runningJob = Optional.empty();

    @Override
    public void execute() throws Exception {

        List<CancellableJob> subJobs = List.of(
                new ExportDocumentsToArchiveJob(),
                new IndexArchivesJob(),
                new RecoverArchiveJob());

        for (CancellableJob currentJob : subJobs) {

            runningJob = Optional.of(currentJob);

            if (isCancelled()) {
                break;
            }

            execute(currentJob);

            runningJob = Optional.empty();
        }
    }

    @Override
    public String cancel() {

        super.cancel();

        if (runningJob.isPresent()) {
            return runningJob.map(CancellableJob::cancel)
                             .orElse(null);
        }

        // No sub job is running, cancel self only
        return null;
    }

}
