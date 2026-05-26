package org.skyve.job;


import java.util.Date;

/**
 * Defines the scheduling contract for a background job execution entry.
 *
 * <p>A schedule identifies the target job ({@link #getJobName()}), the
 * recurrence pattern ({@link #getCronExpression()}), and optional execution
 * bounds ({@link #getStartTime()}, {@link #getEndTime()}). The runtime is
 * expected to evaluate all populated constraints when deciding whether a job
 * instance is eligible to run.
 *
 * <p>Threading: not thread-safe. Instances are mutable data carriers and must
 * be confined to a single configuration/loading thread unless externally
 * synchronized.
 *
 * @since 9.5.0
 */
public class JobSchedule {
	private String uuid;
	private String jobName;
	private Date startTime;
	private Date endTime;
	private String cronExpression;
	
	/**
	 * Returns the unique identifier for this job schedule.
	 * 
	 * @return the UUID of this schedule
	 */
	public String getUuid() {
		return uuid;
	}
	
	/**
	 * Sets the unique identifier for this job schedule.
	 * 
	 * @param uuid the UUID to set
	 */
	public void setUuid(String uuid) {
		this.uuid = uuid;
	}
	
	/**
	 * Returns the name of the job to be scheduled.
	 * 
	 * @return the job name
	 */
	public String getJobName() {
		return jobName;
	}
	
	/**
	 * Sets the name of the job to be scheduled.
	 * 
	 * @param jobName the job name to set
	 */
	public void setJobName(String jobName) {
		this.jobName = jobName;
	}
	
	/**
	 * Returns the earliest time when the job can begin execution.
	 * 
	 * @return the start time, or {@code null} if no start time constraint exists
	 */
	public Date getStartTime() {
		return startTime;
	}
	
	/**
	 * Sets the earliest time when the job can begin execution.
	 * 
	 * @param startTime the start time to set, or {@code null} for no constraint
	 */
	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}
	
	/**
	 * Returns the latest time after which the job should no longer execute.
	 * 
	 * @return the end time, or {@code null} if no end time constraint exists
	 */
	public Date getEndTime() {
		return endTime;
	}
	
	/**
	 * Sets the latest time after which the job should no longer execute.
	 * 
	 * @param endTime the end time to set, or {@code null} for no constraint
	 */
	public void setEndTime(Date endTime) {
		this.endTime = endTime;
	}
	
	/**
	 * Returns the cron expression defining the job's execution schedule.
	 * 
	 * @return the cron expression
	 */
	public String getCronExpression() {
		return cronExpression;
	}
	
	/**
	 * Sets the cron expression defining the job's execution schedule.
	 * 
	 * @param cronExpression the cron expression to set
	 */
	public void setCronExpression(String cronExpression) {
		this.cronExpression = cronExpression;
	}
}
