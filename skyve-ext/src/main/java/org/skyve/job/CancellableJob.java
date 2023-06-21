package org.skyve.job;

/**
 * A job which can be cancelled.
 * If cancelled, and shouldRollbackOnCancel() yields true, the associated transaction is rolled back.
 * Care should be taken if this method yields false that the data store is in a valid state at the cancellation point before commit.
 * ie if false, the Job needs to be idemptotent, and data store changes need to be atomic and consistent.
 */
public abstract class CancellableJob extends Job {
	// Ensures state is flushed to memory to be shared between chip cores.
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
