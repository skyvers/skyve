package org.skyve.job;

import org.skyve.impl.job.AbstractSkyveJob;

public abstract class Job extends AbstractSkyveJob {
	/**
	 * Implement the job here.
	 */
	@Override
	public abstract void execute() throws Exception;

	/**
	 * Cancel the job here.
	 * If the job can't be cancelled then return a reason why the job cannot be cancelled, otherwise return null.
	 */
	@Override
	public String cancel() {
		return "Job cannot be cancelled.";
	}
	
	/**
	 * Indicates whether to rollback the associated transaction when a job is cancelled.
	 * Job Implementations can override this to false if required.
	 * Care should be taken if this method yields false that the data store is in a valid state at the cancellation point before commit.
	 * ie if false, the Job needs to be idemptotent, and data store changes need to be atomic and consistent.
	 * @return	the default, true.
	 */
	@Override
	public boolean shouldRollbackOnCancel() {
		return true;
	}

	/**
	 * execute another job as part of this job in the same thread.
	 */
	@Override
	public final void execute(Job job) throws Exception {
		job.setBean(getBean());
		job.setLog(getLog());
		job.execute();
	}
}
