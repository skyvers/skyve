package org.skyve.impl.web;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.web.BackgroundTask;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Implements the cacheConversation(), background() & backgroundWithoutCachingConversation() methods.
 * @author mike
 */
public abstract class ViewWebContext extends AbstractWebContext {
	private static final long serialVersionUID = 3308226433681394241L;

	protected ViewWebContext(String key, HttpServletRequest request, Object response) {
		super(key, request, response);
		// Set the sessionId
		HttpSession session = request.getSession(false);
		if (session == null) {
			throw new SessionEndedException(request.getLocale());
		}
		sessionId = session.getId();
	}

	@Override
	public void cacheConversation() throws Exception {
		StateUtil.cacheConversation(this);
	}
	
	@Override
	public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		cacheConversation();
		EXT.getJobScheduler().runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}

	@Override
	public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		EXT.getJobScheduler().runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}
}
