package org.skyve.job;

import org.skyve.wildcat.job.AbstractWildcatJob;

public abstract class WildcatJob extends AbstractWildcatJob {
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
	public abstract String cancel();
}
