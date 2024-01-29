package org.skyve.impl.web.faces;

import jakarta.faces.context.ExceptionHandler;
import jakarta.faces.context.ExceptionHandlerFactory;

public class SkyveExceptionHandlerFactory extends ExceptionHandlerFactory {
	public SkyveExceptionHandlerFactory(ExceptionHandlerFactory base) {
		super(base);
	}

	@Override
	public ExceptionHandler getExceptionHandler() {
		return new SkyveExceptionHandler(getWrapped().getExceptionHandler());
	}
}
