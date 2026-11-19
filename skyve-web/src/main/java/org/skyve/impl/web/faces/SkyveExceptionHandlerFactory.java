package org.skyve.impl.web.faces;

import jakarta.annotation.Nonnull;
import jakarta.faces.context.ExceptionHandler;
import jakarta.faces.context.ExceptionHandlerFactory;

/**
 * Implements internal web-module behavior for this Skyve runtime concern.
 */
public class SkyveExceptionHandlerFactory extends ExceptionHandlerFactory {
	/**
	 * Creates a factory that wraps the default JSF exception handler with Skyve behaviour.
	 *
	 * @param base the JSF exception-handler factory being wrapped
	 */
	public SkyveExceptionHandlerFactory(@Nonnull ExceptionHandlerFactory base) {
		super(base);
	}

	/**
	 * Returns a Skyve exception handler that wraps the current JSF handler chain.
	 *
	 * @return the wrapped Skyve-aware exception handler instance
	 */
	@Override
	public @Nonnull ExceptionHandler getExceptionHandler() {
		return new SkyveExceptionHandler(getWrapped().getExceptionHandler());
	}
}
