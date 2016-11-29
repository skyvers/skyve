package org.skyve.impl.web.faces;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.el.ValueExpression;
import javax.faces.FacesException;
import javax.faces.application.FacesMessage;
import javax.faces.application.ViewExpiredException;
import javax.faces.component.UIComponent;
import javax.faces.context.ExceptionHandler;
import javax.faces.context.ExceptionHandlerWrapper;
import javax.faces.context.FacesContext;
import javax.faces.event.ExceptionQueuedEvent;
import javax.faces.event.ExceptionQueuedEventContext;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.impl.persistence.AbstractPersistence;

public class SkyveExceptionHandler extends ExceptionHandlerWrapper {
	private ExceptionHandler wrapped;

	public SkyveExceptionHandler(ExceptionHandler wrapped) {
		this.wrapped = wrapped;
	}

	@Override
	public ExceptionHandler getWrapped() {
		return this.wrapped;
	}

	@Override
	public void handle() throws FacesException {
		Iterator<ExceptionQueuedEvent> eventsIterator = this.wrapped.getUnhandledExceptionQueuedEvents().iterator();
		while (eventsIterator.hasNext()) {
			ExceptionQueuedEvent event = eventsIterator.next();
			ExceptionQueuedEventContext eventContext = event.getContext();
			FacesContext fc = eventContext.getContext();
			Throwable t = eventContext.getException();
			t.printStackTrace();
			AbstractPersistence.get().rollback();
			
			if (t instanceof FacesException) {
				if (t instanceof ViewExpiredException) {
					eventsIterator.remove();
					throw (FacesException) t;
				}

				// Unwind faces exceptions
				Throwable c = t.getCause();
				while (c instanceof FacesException) {
					c = c.getCause();
				}
				if (c != null) {
					t = c;
				}
			}
			if (t instanceof InvocationTargetException) {
				t = ((InvocationTargetException) t).getTargetException();
			}

			if (t instanceof MessageException) {
				for (Message em : ((MessageException) t).getMessages()) {
					processErrors(fc, em);
				}
			}
			else {
				FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, t.getMessage(), t.getMessage());
		        fc.addMessage(null, msg);
			}
		}

//		this.wrapped.handle();
	}
	
	/**
	 * If the message is bound, add it globally and add it to all it's bindings.
	 * If the message is not bound, just add it globally.
	 * The approach relies on the template.xhtmls having something like...
	 * 
	 * <p:messages id="messages" globalOnly="true" autoUpdate="true" />
	 * <p:growl id="growl" globalOnly="true" showDetail="false" autoUpdate="true" />
	 * 
	 * and
	 * 
	 * <p:message for="<binding>" on each input widget.
	 * Adding messages manually for each type of message display mechanism affords the most flexibility.
	 * 
	 * @param context
	 * @param em
	 */
	private static void processErrors(FacesContext context, Message em) {
		String message = em.getErrorMessage();
		FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message);
		for (String binding : em.getBindings()) {
			List<UIComponent> components = findComponentsByBinding(context.getViewRoot(), binding);
			for (UIComponent component : components) {
				if (component.isRendered()) {
					context.addMessage(component.getClientId(), msg);
				}
			}
		}
		context.addMessage(null, msg);
	}
	
	private static List<UIComponent> findComponentsByBinding(UIComponent base, String binding) {
		List<UIComponent> result = new ArrayList<>();
		findComponentsByBinding(base, binding, result);
		return result;
	}

	private static void findComponentsByBinding(UIComponent base, String binding, List<UIComponent> result) {
		ValueExpression ve = base.getValueExpression("value");
		if (ve != null) {
			String expression = ve.getExpressionString();
			int bindingIndex = expression.indexOf(binding);
			// found a match if binding is at the end of the expression (minus the last "}" or minus the last "']}" or minus the last "}']}")
			if (bindingIndex > 0) { // found a match somewhere
				int endBindingIndex = bindingIndex + binding.length();
				int expressionLength = expression.length();
				if ((endBindingIndex == expressionLength - 1) || // "}" standard expression
						(endBindingIndex == expressionLength - 3) || // "']}" BeanMapAdapter binding expression
						(endBindingIndex == expressionLength - 4)) { // "}']} BeanMapAdapter binding replacement expression
					result.add(base);
				}
			}
		}

		Iterator<UIComponent> kids = base.getFacetsAndChildren();
		while (kids.hasNext()) {
			findComponentsByBinding(kids.next(), binding, result);
		}
	}
}
