package org.skyve.impl.web;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.web.BackgroundTask;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Implements the cacheConversation(), background() & backgroundWithoutCachingConversation() methods.
 * @author mike
 */
public abstract class ViewWebContext extends AbstractWebContext {
	private static final long serialVersionUID = 3308226433681394241L;

	/**
	 * Creates a view web context bound to the active HTTP session.
	 *
	 * @param key the web-context cache key
	 * @param request the request used to resolve the active session
	 */
	protected ViewWebContext(@Nonnull String key, @Nonnull HttpServletRequest request) {
		super(key);
		// Set the sessionId
		HttpSession session = request.getSession(false);
		if (session == null) {
			throw new SessionEndedException(request.getLocale());
		}
		sessionId = session.getId();
	}

	@Override
	public void cacheConversationAndCycleTransaction() {
		StateUtil.commitAndCacheConversation(this);
		// Public action code may continue after caching, so give it a new unit of work.
		AbstractPersistence p = getConversation();
		if (p != null) {
			p.begin();
		}
	}
	
	/**
	 * Caches the conversation and schedules the supplied background task.
	 *
	 * @param taskClass the background task type to execute
	 * @param <T> the bean type used by the background task
	 */
	@Override
	public <T extends Bean> void background(@Nonnull Class<? extends BackgroundTask<T>> taskClass) {
		cacheConversationAndCycleTransaction();
		EXT.getJobScheduler().runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}

	/**
	 * Schedules the supplied background task without caching the current conversation.
	 *
	 * @param taskClass the background task type to execute
	 * @param <T> the bean type used by the background task
	 */
	@Override
	public <T extends Bean> void backgroundWithoutCachingConversation(@Nonnull Class<? extends BackgroundTask<T>> taskClass) {
		EXT.getJobScheduler().runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}
}
