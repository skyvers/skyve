package modules.admin.Jobs;

import java.util.List;

import org.skyve.EXT;
import org.skyve.job.JobDescription;

import jakarta.enterprise.inject.Default;
import modules.admin.Job.JobExtension;
import modules.admin.domain.Job;
import modules.admin.domain.Jobs;

/**
 * Provides service operations for populating running job information.
 */
@Default
public class JobsService {
	/**
	 * Refreshes the list of running jobs for the given `Jobs` instance.
	 * This method clears the current list of running jobs and repopulates it
	 * with the jobs that are currently running in the system.
	 *
	 * @param jobs
	 *        the jobs instance whose running-jobs collection should be refreshed
	 * @throws Exception
	 *         if querying the scheduler fails
	 */
	@SuppressWarnings("static-method")
	public final void refresh(Jobs jobs) {
		List<JobExtension> runningJobs = jobs.getRunningJobs();
		runningJobs.clear();

		for (JobDescription jd : EXT.getJobScheduler().getCustomerRunningJobs()) {
			// the job could be finished but the thread is still sleeping waiting for the last UI poll
			if (jd.getStatus() == null) { // not finished
				JobExtension job = Job.newInstance();
				job.setStartTime(jd.getStartTime());
				job.setDisplayName(jd.getName());
				job.setPercentComplete(Integer.valueOf(jd.getPercentComplete()));
				job.setLog(jd.getLogging());
				job.setInstanceId(jd.getInstanceId());
				runningJobs.add(job);
			}
		}
	}
}
