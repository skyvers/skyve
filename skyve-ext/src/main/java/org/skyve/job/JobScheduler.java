package org.skyve.job;

import java.util.Date;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.util.SystemObserver;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides scheduling and management capabilities for jobs, reports, and background tasks.
 * <p>
 * This interface defines methods for running one-shot jobs, scheduling recurring jobs and reports,
 * managing background tasks, and handling system operations like restore and content garbage collection.
 * </p>
 * 
 * @author Skyve
 * @since 9.5.0
 */
public interface JobScheduler extends SystemObserver {
	/**
	 * Run a job once. 
	 * The job disappears from the Scheduler once it is run and a record of the run in placed in admin.Job. 
	 * User must look in admin to see if job was successful.
	 * 
	 * @param job The job to run
	 * @param parameter 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 */
	void runOneShotJob(@Nonnull JobMetaData job, @Nullable Bean parameter, @Nonnull User user);

	/**
	 * Extra parameter gives polling UIs the chance to display the results of the job.
	 * 
	 * @param job The job to run
	 * @param parameter 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 * @param sleepAtEndInSeconds Set this 5 secs higher than the polling time of the UI
	 * @throws Exception
	 */
	void runOneShotJob(@Nonnull JobMetaData job, @Nullable Bean parameter, @Nonnull User user, int sleepAtEndInSeconds);
	
	/**
	 * Run a Background task.
	 * 
	 * @param taskClass	The job to run
	 * @param user	The current user
	 * @param webId	The webId of the conversation to get from the cache
	 */
	<T extends Bean> void runBackgroundTask(@Nonnull Class<? extends BackgroundTask<T>> taskClass,
												@Nonnull User user,
												@Nonnull String webId);
	
	/**
	 * Run a job once at a certain date and time. 
	 * The job disappears from the Scheduler once it is run and a record of the run in placed in admin.Job. 
	 * User must look in admin to see if job was successful.
	 * 
	 * @param job The job to run
	 * @param parameter 	The job parameter - can be null.
	 * @param user	The user to run the job as.
	 * @param when	The date/time to run the job at.
	 */
	void scheduleOneShotJob(@Nonnull JobMetaData job,
								@Nullable Bean parameter,
								@Nonnull User user,
								@Nonnull Date when);
	
	/**
	 * Schedules a job for recurring execution based on the provided {@link JobSchedule}.
	 * <p>
	 * The job will be executed according to the cron expression and time boundaries
	 * defined in the schedule, running under the context of the specified user.
	 * </p>
	 * 
	 * @param jobSchedule the schedule configuration containing cron expression and time constraints
	 * @param user the user under whose context the job will execute
	 */
	void scheduleJob(@Nonnull JobSchedule jobSchedule, @Nonnull User user);

	/**
	 * Removes a scheduled job for the specified customer, if it is scheduled.
	 * <p>
	 * This stops any future executions of the job identified by the given UUID.
	 * </p>
	 * 
	 * @param uuid the unique identifier of the job schedule to remove
	 * @param customerName the name of the customer whose job schedule should be removed
	 */
	void unscheduleJob(@Nonnull String uuid, @Nonnull String customerName);

	/**
	 * Schedules a report for recurring execution based on the provided {@link JobSchedule}.
	 * <p>
	 * The report will be generated according to the cron expression and time boundaries
	 * defined in the schedule, running under the context of the specified user.
	 * </p>
	 * 
	 * @param reportSchedule the schedule configuration containing cron expression and time constraints
	 * @param user the user under whose context the report will execute
	 */
	void scheduleReport(@Nonnull JobSchedule reportSchedule, @Nonnull User user);

	/**
	 * Removes a scheduled report for the specified customer, if it is scheduled.
	 * <p>
	 * This stops any future executions of the report identified by the given UUID.
	 * </p>
	 * 
	 * @param uuid the unique identifier of the report schedule to remove
	 * @param customerName the name of the customer whose report schedule should be removed
	 */
	void unscheduleReport(@Nonnull String uuid, @Nonnull String customerName);

	/**
	 * Returns a list of currently executing jobs, reports, and background tasks for the current customer.
	 * 
	 * @return a list of {@link JobDescription} objects representing currently running jobs, never null
	 */
	@Nonnull List<JobDescription> getCustomerRunningJobs();

	/**
	 * Cancels a currently executing job, report, or background task.
	 * 
	 * @param instanceId the unique identifier of the executing job instance
	 * @return {@code true} if the cancellation was successful, {@code false} otherwise
	 */
	boolean cancelJob(String instanceId);

	/**
	 * Prepares the scheduler for a restore operation.
	 * <p>
	 * This method pauses and unschedules jobs that could interfere with the restore process.
	 * It should be called before the restore job is executed.
	 * </p>
	 */
	void preRestore();
	
	/**
	 * Completes the restore operation and restores the scheduler state.
	 * <p>
	 * This method is called after the restore job completes. If the restore was successful,
	 * it reloads the customer's job schedules. Regardless of success or failure,
	 * system jobs are resumed.
	 * </p>
	 * 
	 * @param restoreSuccessful {@code true} if the restore completed successfully, {@code false} otherwise
	 */
	void postRestore(boolean restoreSuccessful);
	
	/**
	 * Executes the restore job with the specified options.
	 * 
	 * @param restoreOptions the bean implementing RestoreOptions that configures the restore operation
	 */
	void runRestoreJob(Bean restoreOptions);
	
	/**
	 * Performs a complete restore operation with proper scheduler management.
	 * <p>
	 * This method orchestrates the restore process by:
	 * <ol>
	 *   <li>Pausing system jobs and unscheduling customer jobs via {@link #preRestore()}</li>
	 *   <li>Executing the restore job via {@link #runRestoreJob(Bean)}</li>
	 * </ol>
	 * The {@link #postRestore(boolean)} method should be called after the restore job completes
	 * to resume normal scheduler operation.
	 * </p>
	 * 
	 * @param restoreOptions the bean implementing RestoreOptions that configures the restore operation
	 */
	default void restore(Bean restoreOptions) {
		preRestore();
		runRestoreJob(restoreOptions);
	}

	/**
	 * Validates all Skyve meta-data for all customers asynchronously.
	 * This is called by Skyve at startup, after all customer Observer.startup() callbacks
	 * to enable adjustment of repository configuration to occur before complete meta-data validation.
	 * Any validation errors are logged but do no abort Skyve application deployment. 
	 */
	void validateMetaData();

	/**
	 * Run Content Garbage Collector.
	 */
	void runContentGarbageCollector();
}
