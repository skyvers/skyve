package org.skyve.impl.cdi;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.job.JobDescription;
import org.skyve.job.JobSchedule;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

import jakarta.enterprise.inject.Alternative;

/**
 * Stateless CDI proxy for {@link JobScheduler}.
 *
 * <p>Delegates to {@link EXT#getJobScheduler()} so scheduled job execution remains
 * available after bean passivation/activation.
 */
@Alternative
public class JobSchedulerInjectable implements JobScheduler, Serializable {
	private static final long serialVersionUID = 7634387574414670560L;

	/**
	 * Starts scheduler infrastructure and recurring triggers.
	 */
	@Override
	public void startup() {
		EXT.getJobScheduler().startup();
	}

	/**
	 * Stops the scheduler and releases scheduling resources.
	 */
	@Override
	public void shutdown() {
		EXT.getJobScheduler().shutdown();
	}

	/**
	 * Runs a one-shot job immediately using the supplied parameter bean and user context.
	 *
	 * @param job the job metadata to execute.
	 * @param parameter the parameter bean supplied to the job.
	 * @param user the user context for execution.
	 */
	@Override
	public void runOneShotJob(JobMetaData job, Bean parameter, User user) {
		EXT.getJobScheduler().runOneShotJob(job, parameter, user);
	}

	/**
	 * Runs a one-shot job immediately and keeps the worker alive for the specified delay.
	 *
	 * @param job the job metadata to execute.
	 * @param parameter the parameter bean supplied to the job.
	 * @param user the user context for execution.
	 * @param sleepAtEndInSeconds the post-execution delay in seconds.
	 */
	@Override
	public void runOneShotJob(JobMetaData job, Bean parameter, User user, int sleepAtEndInSeconds) {
		EXT.getJobScheduler().runOneShotJob(job, parameter, user, sleepAtEndInSeconds);
	}

	/**
	 * Runs a background task implementation under the supplied user/web context.
	 *
	 * @param taskClass the background task class to run.
	 * @param user the user context for execution.
	 * @param webId the web context identifier.
	 * @param <T> the bean type handled by the background task.
	 */
	@Override
	public <T extends Bean> void runBackgroundTask(Class<? extends BackgroundTask<T>> taskClass, User user, String webId) {
		EXT.getJobScheduler().runBackgroundTask(taskClass, user, webId);
	}

	/**
	 * Triggers content garbage collection as an immediate scheduler task.
	 */
	@Override
	public void runContentGarbageCollector() {
		EXT.getJobScheduler().runContentGarbageCollector();
	}
	
	/**
	 * Schedules a one-shot job for execution at {@code when}.
	 *
	 * @param job the job metadata to schedule.
	 * @param parameter the parameter bean supplied to the job.
	 * @param user the user context for execution.
	 * @param when the scheduled execution time.
	 */
	@Override
	public void scheduleOneShotJob(JobMetaData job, Bean parameter, User user, Date when) {
		EXT.getJobScheduler().scheduleOneShotJob(job, parameter, user, when);
	}

	/**
	 * Schedules a recurring or delayed job definition.
	 *
	 * @param jobSchedule the schedule definition to register.
	 * @param user the user context used for scheduling.
	 */
	@Override
	public void scheduleJob(JobSchedule jobSchedule, User user) {
		EXT.getJobScheduler().scheduleJob(jobSchedule, user);
	}

	/**
	 * Removes a previously scheduled job definition.
	 *
	 * @param uuid the scheduled job identifier.
	 * @param customerName the owning customer name.
	 */
	@Override
	public void unscheduleJob(String uuid, String customerName) {
		EXT.getJobScheduler().unscheduleJob(uuid, customerName);
	}

	/**
	 * Schedules a report job definition.
	 *
	 * @param reportSchedule the report schedule definition to register.
	 * @param user the user context used for scheduling.
	 */
	@Override
	public void scheduleReport(JobSchedule reportSchedule, User user) {
		EXT.getJobScheduler().scheduleReport(reportSchedule, user);
	}

	/**
	 * Removes a previously scheduled report definition.
	 *
	 * @param uuid the scheduled report identifier.
	 * @param customerName the owning customer name.
	 */
	@Override
	public void unscheduleReport(String uuid, String customerName) {
		EXT.getJobScheduler().unscheduleReport(uuid, customerName);
	}

	/**
	 * Returns currently running jobs visible to the active customer context.
	 *
	 * @return running jobs for the current customer context.
	 */
	@Override
	public List<JobDescription> getCustomerRunningJobs() {
		return EXT.getJobScheduler().getCustomerRunningJobs();
	}

	/**
	 * Attempts cancellation of a running job instance.
	 *
	 * @param instanceId the running job instance identifier.
	 * @return true if cancellation was requested successfully; otherwise false.
	 */
	@Override
	public boolean cancelJob(String instanceId) {
		return EXT.getJobScheduler().cancelJob(instanceId);
	}
	
	/**
	 * Validates scheduler metadata and configuration.
	 */
	@Override
	public void validateMetaData() {
		EXT.getJobScheduler().validateMetaData();
	}

	/**
	 * Executes pre-restore scheduler coordination hooks.
	 */
	@Override
	public void preRestore() {
		EXT.getJobScheduler().preRestore();
	}

	/**
	 * Runs the restore job using supplied restore options.
	 *
	 * @param restoreOptions the restore options bean.
	 */
	@Override
	public void runRestoreJob(Bean restoreOptions) {
		EXT.getJobScheduler().preRestore();
	}
	
	/**
	 * Finalizes scheduler restore lifecycle processing.
	 *
	 * @param restoreSuccessful true if restore completed successfully; otherwise false.
	 */
	@Override
	public void postRestore(boolean restoreSuccessful) {
		EXT.getJobScheduler().postRestore(restoreSuccessful);
	}
}
