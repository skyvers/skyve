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
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.Job;

public class RerunJob implements ServerSideAction<Job> {

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
	 * @return True if the job is running, false otherwise
	 * @throws Exception
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
			Util.LOGGER.warning("Error getting running jobs");
			e.printStackTrace();
		}

		return false;
	}
}
