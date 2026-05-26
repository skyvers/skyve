package org.skyve.job;

/**
 * Terminal states for a background job execution.
 *
 * <p>These values are written to job status records and used by admin/job monitoring
 * views to indicate completion outcome.
 */
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
