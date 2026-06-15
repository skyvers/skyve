package org.skyve.web;

import org.skyve.domain.Bean;

/**
 * Defines a short-lived asynchronous unit of work initiated from a web
 * conversation context.
 *
 * <p>Implementations execute against a conversation bean of type {@code T}
 * and may request conversation persistence via {@link #cacheConversation()}.
 * Implementations should assume execution outside the request thread and
 * therefore avoid request-scoped state that is not explicitly captured.
 *
 * <p>Threading: implementations are expected to be thread-confined per task
 * instance; shared mutable state requires external synchronization.
 *
 * @param <T> the bean type operated on by the task, typically the
 *            conversation bean
 */
public interface BackgroundTask<T extends Bean> {
	/**
	 * Returns the bean snapshot/context for this task execution.
	 *
	 * @return the task bean, never {@code null}
	 */
	T getBean();

	/**
	 * Persists or places the backing conversation into the conversation cache.
	 *
	 * <p>Side effects: may write conversation state to cache/storage.
	 *
	 * @throws Exception if caching fails
	 */
	void cacheConversation() throws Exception;

	/**
	 * Executes task-specific background work.
	 *
	 * <p>Precondition: {@code bean} is the same logical context returned by
	 * {@link #getBean()}.
	 *
	 * @param bean the task bean/context
	 * @throws Exception if execution fails
	 */
	void execute(T bean) throws Exception;
}
