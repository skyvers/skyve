package org.skyve.job;

import javax.inject.Inject;

import org.skyve.impl.job.AbstractSkyveJob;
import org.skyve.web.Pusher;
import org.skyve.web.Pusher.PushMessage;

public abstract class Job extends AbstractSkyveJob {
	private static final long serialVersionUID = 1333768405570850256L;

	@Inject
	Pusher pusher;
	
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
	
	/**
	 * PushMessage factory method
	 * @return	A new push message.
	 */
	public final PushMessage newPushMessage() {
		return pusher.newPushMessage();
	}
	
	/**
	 * This method is used to push to clients.
	 * @param message	The message to push.
	 */
	public final void push(PushMessage message) throws Exception {
		pusher.push(message);
	}
}
