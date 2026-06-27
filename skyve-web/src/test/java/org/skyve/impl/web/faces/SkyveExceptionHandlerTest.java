package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;

import jakarta.faces.FacesException;
import jakarta.faces.context.ExceptionHandler;
import jakarta.faces.event.ExceptionQueuedEvent;
import jakarta.faces.event.ExceptionQueuedEventContext;

@SuppressWarnings("static-method")
class SkyveExceptionHandlerTest {
	@Test
	void handleRethrowsRuntimeExceptionFromFirstUnhandledEvent() {
		ExceptionHandler wrapped = mock(ExceptionHandler.class);
		ExceptionQueuedEvent event = mock(ExceptionQueuedEvent.class);
		ExceptionQueuedEventContext eventContext = mock(ExceptionQueuedEventContext.class);
		RuntimeException runtime = new RuntimeException("boom");
		when(wrapped.getUnhandledExceptionQueuedEvents()).thenReturn(List.of(event));
		when(event.getContext()).thenReturn(eventContext);
		when(eventContext.getException()).thenReturn(runtime);

		SkyveExceptionHandler handler = new SkyveExceptionHandler(wrapped);
		RuntimeException thrown = assertThrows(RuntimeException.class, handler::handle);

		assertSame(runtime, thrown);
		verify(wrapped, never()).handle();
	}

	@Test
	void handleWrapsNonRuntimeThrowableInFacesException() {
		ExceptionHandler wrapped = mock(ExceptionHandler.class);
		ExceptionQueuedEvent event = mock(ExceptionQueuedEvent.class);
		ExceptionQueuedEventContext eventContext = mock(ExceptionQueuedEventContext.class);
		Exception checked = new Exception("checked");
		when(wrapped.getUnhandledExceptionQueuedEvents()).thenReturn(List.of(event));
		when(event.getContext()).thenReturn(eventContext);
		when(eventContext.getException()).thenReturn(checked);

		SkyveExceptionHandler handler = new SkyveExceptionHandler(wrapped);
		FacesException thrown = assertThrows(FacesException.class, handler::handle);

		assertSame(checked, thrown.getCause());
		verify(wrapped, never()).handle();
	}

	@Test
	void handleDelegatesWhenThereAreNoUnhandledEvents() {
		ExceptionHandler wrapped = mock(ExceptionHandler.class);
		when(wrapped.getUnhandledExceptionQueuedEvents()).thenReturn(List.of());

		SkyveExceptionHandler handler = new SkyveExceptionHandler(wrapped);
		handler.handle();

		verify(wrapped).handle();
	}
}
