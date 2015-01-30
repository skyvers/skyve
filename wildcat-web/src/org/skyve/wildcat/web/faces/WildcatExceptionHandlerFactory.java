package org.skyve.wildcat.web.faces;

import javax.faces.context.ExceptionHandler;
import javax.faces.context.ExceptionHandlerFactory;

public class WildcatExceptionHandlerFactory extends ExceptionHandlerFactory {
	private ExceptionHandlerFactory base;

	public WildcatExceptionHandlerFactory(ExceptionHandlerFactory base) {
		this.base = base;
	}

	@Override
	public ExceptionHandler getExceptionHandler() {
		return new WildcatExceptionHandler(base.getExceptionHandler());
	}
}
