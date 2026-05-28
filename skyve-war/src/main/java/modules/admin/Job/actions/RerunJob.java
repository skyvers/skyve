package modules.admin.Job.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.job.JobDescription;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import modules.admin.domain.Job;

/**
 * Re-runs a completed job when a unique metadata job definition can be resolved.
 */
public class RerunJob implements ServerSideAction<Job> {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(RerunJob.class);

	/**
	 * Resolves the metadata job by display name and runs it immediately.
	 *
	 * @param bean
	 *        the selected historical job entry
	 * @param webContext
	 *        the current web context used for growl feedback
	 * @return a result wrapping {@code bean}
	 */
	@Override
	public ServerSideActionResult<Job> execute(Job bean, WebContext webContext) {
		// try to find the job to re-run, based on unique display name
		JobMetaData jobToRerun = null;
		Customer c = CORE.getCustomer();
		List<org.skyve.metadata.module.Module> modules = c.getModules();
		for (org.skyve.metadata.module.Module m : modules) {
			List<JobMetaData> jobs = m.getJobs();
			for (JobMetaData j : jobs) {
				if (j.getDisplayName().equals(bean.getDisplayName())) {
					jobToRerun = j;
					break;
				}
			}

			if (jobToRerun != null) {
				break;
			}
		}

		if (jobToRerun != null) {
			if (isJobAlreadyActive(jobToRerun)) {
				webContext.growl(MessageSeverity.warn, "This job is already running");
			} else {
				// start the job immediately as the current user
				EXT.getJobScheduler().runOneShotJob(jobToRerun, bean, CORE.getUser());
				webContext.growl(MessageSeverity.info, "Started " + jobToRerun.getDisplayName());
			}
		} else {
			webContext.growl(MessageSeverity.warn, "Unable to find this job to re-run");
		}

		return new ServerSideActionResult<>(bean);
	}

	/**
	 * Checks if the current job is currently running.
	 *
	 * @param job
	 *        the job metadata to test
	 *
	 * @return {@code true} if the job is running, otherwise {@code false}
	 */
	private static boolean isJobAlreadyActive(final JobMetaData job) {
		try {
			List<JobDescription> runningJobs = EXT.getJobScheduler().getCustomerRunningJobs();
			for (JobDescription jd : runningJobs) {
				if (job.getDisplayName().equals(jd.getName())) {
					return true;
				}
			}
		} catch (Exception e) {
			LOGGER.warn("Error getting running jobs", e);
		}

		return false;
	}
}
