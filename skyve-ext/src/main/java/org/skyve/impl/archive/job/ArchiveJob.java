package org.skyve.impl.archive.job;

import java.util.List;
import java.util.Optional;

import org.skyve.job.CancellableJob;

public class ArchiveJob extends CancellableJob {

    private Optional<CancellableJob> runningJob = Optional.empty();

    @Override
    public void execute() throws Exception {

        List<CancellableJob> subJobs = List.of(
                new ExportDocumentsToArchiveJob(),
                new IndexArchivesJob());

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
