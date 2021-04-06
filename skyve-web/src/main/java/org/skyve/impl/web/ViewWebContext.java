package org.skyve.impl.web;

import org.skyve.cache.StateUtil;
import org.skyve.domain.Bean;
import org.skyve.job.JobScheduler;
import org.skyve.web.BackgroundTask;

/**
 * Implements the cacheConversation(), background() & backgroundWithoutCachingConversation() methods.
 * @author mike
 */
public abstract class ViewWebContext extends AbstractWebContext {
	private static final long serialVersionUID = 3308226433681394241L;

	protected ViewWebContext(String key, Object request, Object response) {
		super(key, request, response);
	}

	@Override
	public void cacheConversation() throws Exception {
		StateUtil.cacheConversation(this);
	}
	
	@Override
	public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		cacheConversation();
		JobScheduler.runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}

	@Override
	public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		JobScheduler.runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}
}
