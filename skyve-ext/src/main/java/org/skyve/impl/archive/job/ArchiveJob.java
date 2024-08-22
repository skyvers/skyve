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

            if (isCancelled()) {
                break;
            }

            runningJob = Optional.of(currentJob);
            execute(currentJob);
            runningJob = Optional.empty();
        }
    }

    @Override
    public String cancel() {

        return runningJob.map(CancellableJob::cancel)
                         .orElse("Unable to cancel");
    }

}
