package org.skyve.impl.web.faces;

import javax.faces.context.ExceptionHandler;
import javax.faces.context.ExceptionHandlerFactory;

public class SkyveExceptionHandlerFactory extends ExceptionHandlerFactory {
	private ExceptionHandlerFactory base;

	public SkyveExceptionHandlerFactory(ExceptionHandlerFactory base) {
		this.base = base;
	}

	@Override
	public ExceptionHandler getExceptionHandler() {
		return new SkyveExceptionHandler(base.getExceptionHandler());
	}
}
