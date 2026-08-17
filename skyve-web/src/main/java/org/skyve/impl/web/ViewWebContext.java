package org.skyve.impl.web;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.web.BackgroundTask;

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
	protected ViewWebContext(String key, HttpServletRequest request) {
		super(key);
		// Set the sessionId
		HttpSession session = request.getSession(false);
		if (session == null) {
			throw new SessionEndedException(request.getLocale());
		}
		sessionId = session.getId();
	}

	@Override
	public void cacheConversation() throws Exception {
		// Commit (which ends the transaction and releases the JDBC connection) BEFORE serialising
		// the conversation, so the Hibernate session is disconnected and serialises cleanly.
		// A server-side action that calls cacheConversation() or background() mid-request runs
		// inside the request's transaction, so the session is otherwise still connected, causing
		// "Cannot serialize SessionImpl while connected" and a corrupted conversation — the same
		// defect SkyveFacesPhaseListener.afterResponseRendered() guards against at render time.
		// Pass false so the EntityManager stays open for the remainder of the request; whichever
		// entry point is servicing the request (the JSF phase listener, a SmartClient servlet etc)
		// performs the final commit(true) in its request cleanup, which closes it.
		AbstractPersistence.get().commit(false);
		StateUtil.cacheConversation(this);
	}
	
	/**
	 * Caches the conversation and schedules the supplied background task.
	 *
	 * @param taskClass the background task type to execute
	 * @param <T> the bean type used by the background task
	 * @throws Exception when caching or scheduling fails
	 */
	@Override
	public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		cacheConversation();
		EXT.getJobScheduler().runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}

	/**
	 * Schedules the supplied background task without caching the current conversation.
	 *
	 * @param taskClass the background task type to execute
	 * @param <T> the bean type used by the background task
	 * @throws Exception when scheduling fails
	 */
	@Override
	public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) throws Exception {
		EXT.getJobScheduler().runBackgroundTask(taskClass, getConversation().getUser(), getWebId());
	}
}
