package org.skyve.job;

import java.util.Date;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.util.SystemObserver;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.BackgroundTask;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public interface JobScheduler extends SystemObserver {
	public static final String REPORTS_GROUP = "REPORTS GROUP";
	
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
	 * Run Content Garbage Collector.
	 */
	void runContentGarbageCollector();

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
	 * Run a job as per the jobSchedule under the given user.
	 * @param jobSchedule	The schedule to the run.
	 * @param user	The user to run the job as.
	 */
	void scheduleJob(@Nonnull Bean jobSchedule, @Nonnull User user);

	/**
	 * Remove this jobSchedule for the given customer.
	 * @param jobSchedule	To be removed
	 * @param customer	The customer to unschedule for.
	 */
	void unscheduleJob(@Nonnull Bean jobSchedule, @Nonnull Customer customer);

	/**
	 * Add a report job by report name to the Job Schedule.
	 * Note this does not schedule to the job.
	 * @param reportName	The report name to add.
	 */
	void addReportJob(@Nonnull String reportName);

	/**
	 * Run a report as per the reportSchedule under the given user.
	 * @param reportSchedule	The schedule to the run.
	 * @param user	The user to run the report as.
	 */
	void scheduleReport(@Nonnull Bean reportSchedule, @Nonnull User user);

	/**
	 * Remove this reportSchedule for the given customer.
	 * @param reportSchedule	To be removed
	 * @param customer	The customer to unschedule for.
	 */
	void unscheduleReport(@Nonnull Bean reportSchedule, @Nonnull Customer customer);

	/**
	 * Get a list of currently executing jobs, reports and background tasks for the current customer.
	 * @return	The list.
	 */
	@Nonnull List<JobDescription> getCustomerRunningJobs();

	/**
	 * Cancel a job, report, background task by its executing instanceId.
	 * @param instanceId	Identifies the executing job.
	 * @return	true if cancellation was successful, otherwise false.
	 */
	boolean cancelJob(String instanceId);
	
	/**
	 * Validates all Skyve meta-data for all customers asynchronously.
	 * This is called by Skyve at startup, after all customer Observer.startup() callbacks
	 * to enable adjustment of repository configuration to occur before complete meta-data validation.
	 * Any validation errors are logged but do no abort Skyve application deployment. 
	 */
	void validateMetaData();
}
