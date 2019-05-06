package org.skyve.web;

import org.skyve.domain.Bean;

/**
 * This is a short-lived view-oriented background task that can be kicked off from WebContext.
 * @author mike
 *
 * @param <T>	The type of bean the task is operating on - usually the conversation bean.
 */
public interface BackgroundTask<T extends Bean> {
	/**
	 * Get the bean for the task.
	 */
	T getBean();

	/**
	 * Place the conversation backing this task into the conversation cache.
	 */
	void cacheConversation() throws Exception;

	/**
	 * Override this to do whatever is required in this task.
	 * @param bean	The same bean returned by getBean() for convenience.
	 */
	void execute(T bean) throws Exception;
}
