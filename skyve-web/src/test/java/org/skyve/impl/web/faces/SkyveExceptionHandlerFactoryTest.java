package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import jakarta.faces.context.ExceptionHandler;
import jakarta.faces.context.ExceptionHandlerFactory;

@SuppressWarnings("static-method")
class SkyveExceptionHandlerFactoryTest {
	@Test
	void getExceptionHandlerWrapsBaseFactoryHandler() {
		ExceptionHandlerFactory base = mock(ExceptionHandlerFactory.class);
		ExceptionHandler wrapped = mock(ExceptionHandler.class);
		when(base.getExceptionHandler()).thenReturn(wrapped);

		SkyveExceptionHandlerFactory factory = new SkyveExceptionHandlerFactory(base);
		ExceptionHandler result = factory.getExceptionHandler();

		SkyveExceptionHandler skyveHandler = assertInstanceOf(SkyveExceptionHandler.class, result);
		assertSame(wrapped, skyveHandler.getWrapped());
	}
}
