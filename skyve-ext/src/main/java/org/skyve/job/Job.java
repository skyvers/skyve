package org.skyve.job;

import org.skyve.impl.job.AbstractSkyveJob;

public abstract class Job extends AbstractSkyveJob {
	private static final long serialVersionUID = 1333768405570850256L;
	
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
	 * execute another job as part of this job in the same thread.
	 */
	@Override
	public final void execute(Job job) throws Exception {
		job.setBean(getBean());
		job.setLog(getLog());
		job.execute();
	}
}
