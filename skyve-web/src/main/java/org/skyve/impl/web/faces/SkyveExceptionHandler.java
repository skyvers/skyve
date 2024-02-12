package org.skyve.impl.web.faces;

import java.util.Iterator;

import jakarta.faces.FacesException;
import jakarta.faces.application.ViewExpiredException;
import jakarta.faces.context.ExceptionHandler;
import jakarta.faces.context.ExceptionHandlerWrapper;
import jakarta.faces.event.ExceptionQueuedEvent;
import jakarta.faces.event.ExceptionQueuedEventContext;

/**
 * Handles ViewExpiredException.
 * Other exceptions during faces processing are handled in FacesAction which
 * will still allow rendering to complete if an exception is thrown during 
 * render response phase.
 * 
 * @author mike
 */
public class SkyveExceptionHandler extends ExceptionHandlerWrapper {
	public SkyveExceptionHandler(ExceptionHandler wrapped) {
		super(wrapped);
	}

	@Override
	public void handle() throws FacesException {
		ExceptionHandler wrapped = getWrapped();
		Iterable<ExceptionQueuedEvent> events = wrapped.getUnhandledExceptionQueuedEvents();
		for (Iterator<ExceptionQueuedEvent> it = events.iterator(); it.hasNext();) {
			ExceptionQueuedEvent event = it.next();
			ExceptionQueuedEventContext eqec = event.getContext();

			if (eqec.getException() instanceof ViewExpiredException) {
				it.remove();
				throw (ViewExpiredException) eqec.getException();
			}
		}

		wrapped.handle();
	}
}
