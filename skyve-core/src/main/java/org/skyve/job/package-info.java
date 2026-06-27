/**
 * Job scheduling value types used by the Skyve job framework.
 *
 * <p>{@link org.skyve.job.JobSchedule} carries the timing configuration for a scheduled
 * job: UUID, name, start/end times, and a cron expression. {@link org.skyve.job.UserJobSchedule}
 * extends this with user-facing scheduling preferences.
 *
 * <p>The full job scheduling API — including submitting, cancelling, and monitoring jobs —
 * is provided by {@link org.skyve.job.JobScheduler} in {@code skyve-ext}.
 */
package org.skyve.job;
