/**
 * Job scheduling and execution API.
 *
 * <p>The central service is {@link org.skyve.job.JobScheduler}, obtained via
 * {@link org.skyve.EXT#getJobScheduler()}. It supports:
 * <ul>
 *   <li>One-shot jobs — run once immediately or at a specified date/time.</li>
 *   <li>Recurring jobs — scheduled via a cron expression in a {@link org.skyve.job.JobSchedule}.</li>
 *   <li>Background tasks — lightweight UI-bound tasks that post progress back to a conversation.</li>
 * </ul>
 *
 * <p>{@link org.skyve.job.Job} is the base class for framework jobs.
 * {@link org.skyve.job.CancellableJob} adds cooperative cancellation support.
 * {@link org.skyve.job.IteratingJob} simplifies bulk-row-processing jobs.
 *
 * @see org.skyve.job.JobScheduler
 * @see org.skyve.job.Job
 */
package org.skyve.job;
