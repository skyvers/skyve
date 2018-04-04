package org.skyve.job;

/**
 * A job which can be cancelled.
 */
public abstract class CancellableJob extends Job {

	private volatile boolean cancelled = false;

	/**
	 * Cancels the job.
	 */
	@Override
	public String cancel() {
		cancelled = true;
		return null;
	}

	/**
	 * Implementations should check call this periodically during their execution.
	 *
	 * @return Whether or not this job should stop its execution.
	 */
	public boolean isCancelled() {
		return cancelled;
	}
}
