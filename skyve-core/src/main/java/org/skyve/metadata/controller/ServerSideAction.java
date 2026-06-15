package org.skyve.metadata.controller;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;

/**
 * The primary Skyve action contract executed when a user activates an action widget.
 *
 * <p>Implementations are wired to action widgets in view metadata via the {@code action}
 * attribute. When the user clicks the button, Skyve pushes the current view state into
 * the bean, instantiates the action class, calls {@link #execute}, and then re-renders
 * the view using the returned bean.
 *
 * <p>The action may replace the returned bean (e.g. navigate to a different document
 * instance) or modify it in-place. Replacing a persistent bean will cause Skyve to
 * update the conversation state and re-navigate.
 *
 * <p>Implementations must be stateless or request-scoped; Skyve creates a new instance
 * per invocation.
 *
 * @param <T>  the document bean type the action operates on
 * @see ServerSideActionResult
 * @see Interceptor#beforeServerSideAction
 */
public interface ServerSideAction<T extends Bean> extends MetaData {
	/**
	 * Executes the action and returns the (possibly updated) bean.
	 *
	 * @param bean        the current bean from the view; never {@code null}
	 * @param webContext  the current web context providing access to conversation and
	 *                    session state; never {@code null}
	 * @return a result wrapping the (potentially replaced) bean; never {@code null}
	 * @throws Exception if the action cannot complete; Skyve will surface the exception
	 *                   to the user as an error message
	 */
	public @Nonnull ServerSideActionResult<T> execute(T bean, WebContext webContext) throws Exception;
}
