package org.skyve.web;

import org.skyve.domain.Bean;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	@Nullable T getBean();

	/**
	 * Commits any active persistence transaction and places the backing conversation
	 * into the conversation cache.
	 *
	 * <p>A replacement transaction is begun before this method returns so background
	 * work can continue. Later failure can roll back only work performed after this
	 * checkpoint.
	 */
	void cacheConversationAndCycleTransaction();

	/**
	 * Executes task-specific background work.
	 *
	 * <p>Precondition: {@code bean} is the same logical context returned by
	 * {@link #getBean()}.
	 *
	 * @param bean the task bean/context
	 * @throws Exception if execution fails
	 */
	@SuppressWarnings("java:S112") // throwing Exception is part of the convenience API
	void execute(@Nonnull T bean) throws Exception;
}
