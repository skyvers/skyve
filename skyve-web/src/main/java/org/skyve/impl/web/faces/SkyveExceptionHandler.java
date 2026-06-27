package org.skyve.impl.web.faces;

import java.util.Iterator;

import jakarta.faces.FacesException;
import jakarta.faces.context.ExceptionHandler;
import jakarta.faces.context.ExceptionHandlerWrapper;
import jakarta.faces.event.ExceptionQueuedEvent;
import jakarta.faces.event.ExceptionQueuedEventContext;

/**
 * Handles unhandled Exceptions by throwing them to the SkyveFacesFilter to be handled.
 * Most exceptions during faces processing are handled in FacesAction which
 * will still allow rendering to complete if an exception is thrown during render response phase.
 * 
 * @author mike
 */
public class SkyveExceptionHandler extends ExceptionHandlerWrapper {
	/**
	 * Creates a wrapper that rethrows queued Faces exceptions for central handling.
	 *
	 * @param wrapped the underlying JSF exception handler
	 */
	public SkyveExceptionHandler(ExceptionHandler wrapped) {
		super(wrapped);
	}

	@Override
	public void handle() throws FacesException {
		ExceptionHandler wrapped = getWrapped();
		Iterator<ExceptionQueuedEvent> it = wrapped.getUnhandledExceptionQueuedEvents().iterator();
		if (it.hasNext()) {
			ExceptionQueuedEvent event = it.next();
			ExceptionQueuedEventContext eqec = event.getContext();
			Throwable unhandled = eqec.getException();
			if (unhandled instanceof RuntimeException re) {
				throw re;
			}
			throw new FacesException(unhandled);
		}

		wrapped.handle();
	}
}
