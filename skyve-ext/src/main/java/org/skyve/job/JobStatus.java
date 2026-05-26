package org.skyve.job;

/**
 * Terminal states for a background job execution.
 *
 * <p>These values are written to job status records and used by admin/job monitoring
 * views to indicate completion outcome.
 */
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum JobStatus {
	/**
	 * Job completed successfully.
	 */
	complete,

	/**
	 * Job terminated with an error.
	 */
	failed,

	/**
	 * Job was cancelled before normal completion.
	 */
	cancelled;
}
