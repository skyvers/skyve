package org.skyve.impl.web.faces;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import javax.el.ValueExpression;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.UIData;
import javax.faces.component.UIInput;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitResult;
import javax.faces.context.FacesContext;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.util.Util;

/**
 * Faces Action provides standard exception handling for Faces processing that
 * occurs in the callback() method.
 * The standard faces exception handling just takes care of view expired exceptions.
 * This standard faces exception handling cannot handle exceptions elegantly half way through render response.
 * It will just provide a blank page.
 * 
 * @author mike
 * @param <T>
 */
public abstract class FacesAction<T> {
	public final T execute() {
		T result = null;
		
		FacesContext fc = FacesContext.getCurrentInstance();
		AbstractPersistence persistence = AbstractPersistence.get();
		try {
			try {
				result = callback();
			}
			catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}
		catch (Throwable t) {
			persistence.setRollbackOnly();
			t.printStackTrace();
			
			if (t instanceof MessageException) {
				TreeSet<String> globalMessageSet = new TreeSet<>();
				for (Message em : ((MessageException) t).getMessages()) {
					processErrors(fc, em, globalMessageSet);
				}
			}
			else {
				FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, t.getMessage(), t.getMessage());
		        fc.addMessage(null, msg);
			}
		}
		
		return result;
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
	private static void processErrors(FacesContext context, Message em, TreeSet<String> globalMessageSet) {
		String message = em.getErrorMessage();
		FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message);
		for (String binding : em.getBindings()) {
			List<UIComponent> components = findComponentsByBinding(context.getViewRoot(), binding);
			for (UIComponent component : components) {
				if (component.isRendered()) {
					context.addMessage(component.getClientId(), msg);
				}
			}

			// find components if this is an indexed compound binding
			// eg listBinding[\d*].simpleBinding or listBinding[\d*].compound.binding etc
			int lastOpeningSquareBraceIndex = binding.lastIndexOf('[');
			int lastClosingSquareBraceIndex = binding.lastIndexOf(']');
			if ((lastOpeningSquareBraceIndex > 0) && 
					(lastClosingSquareBraceIndex > lastOpeningSquareBraceIndex)) {
				if ((lastClosingSquareBraceIndex < (binding.length() - 1)) && // not the end of the binding expression
						(binding.charAt(lastClosingSquareBraceIndex + 1) == '.')) { // followed by a '.' - eg [100].
					String simpleBinding = binding.substring(lastClosingSquareBraceIndex + 2);
					components = findComponentsByBinding(context.getViewRoot(), simpleBinding);

					// We have an indexed binding match that will probably match a data grid
					for (UIComponent component : components) {
						if (component.isRendered()) {
							// We will set s1:s2:s3 to s1:s2:<index>:s3 so it matches the row in the grid
							String clientId = component.getClientId();
							int lastColonIndex = clientId.lastIndexOf(':');
							clientId = String.format("%s:%s%s", 
														clientId.substring(0, lastColonIndex),
														binding.substring(lastOpeningSquareBraceIndex + 1, lastClosingSquareBraceIndex),
														clientId.substring(lastColonIndex));
							context.addMessage(clientId, msg);
						}
					}
				}
			}
		}

		// only add distinct error messages globally
		if (globalMessageSet.add(message)) {
			context.addMessage(null, msg);
		}
	}
	
	public abstract T callback() throws Exception;
	
    public static boolean validateRequiredFields() {
    	TreeSet<String> globalMessages = new TreeSet<>(); // used for removing duplicate global messages
    	FacesContext fc = FacesContext.getCurrentInstance();
    	return validateRequiredFields(fc, fc.getViewRoot(), globalMessages);
    }
    
	private static boolean validateRequiredFields(final FacesContext fc, 
													UIComponent component, 
													final TreeSet<String> globalMessages) {
		boolean result = true;

		// only process this component (and it's children) if it was rendered
		if (component.isRendered()) {
			// If this is a collection based component, iterate it for any UIInput values
			if (component instanceof UIData) {
				UIData data = (UIData) component;
				final MutableBoolean assignedResult = new MutableBoolean(result);
				data.visitTree(VisitContext.createVisitContext(fc), new VisitCallback() {
					@Override
				    @SuppressWarnings("synthetic-access")
				    public VisitResult visit(VisitContext context, UIComponent target) {
				    	if (target instanceof UIInput) {
							// NB ensure && operator doesn't short circuit by putting the function call as the first argument
							// This way we'll get all validation error messages
							assignedResult.setValue(checkRequiredInput(fc, (UIInput) target, globalMessages) && 
														assignedResult.booleanValue());
				        }

				        return VisitResult.ACCEPT;
				    }
				});
				result = assignedResult.booleanValue();
			}
			else { // just check the required values
				if (component instanceof UIInput) {
					result = checkRequiredInput(fc, (UIInput) component, globalMessages);
				}

				// iterate the children (if not in a UIData)
				Iterator<UIComponent> kids = component.getFacetsAndChildren();
				while (kids.hasNext()) {
					// NB ensure && operator doesn't short circuit by putting the function call as the first argument
					// This way we'll get all validation error messages
					result = validateRequiredFields(fc, kids.next(), globalMessages) && result;
				}
			}
		}

		return result;
	}

	private static boolean checkRequiredInput(FacesContext fc, UIInput input, TreeSet<String> globalMessages) {
		String message = input.getRequiredMessage();
		if (Util.processStringValue(message) != null) {
			Object value = input.getValue();
			if ((value == null) || ((value instanceof String) && ((String) value).trim().isEmpty())) {
				FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message);
				fc.addMessage(input.getClientId(), msg);
				
				// Add distinct global messages
				if (globalMessages.add(message)) {
					fc.addMessage(null, msg);
				}
				return false;
			}
		}
		
		return true;
	}

	public static UIComponent findComponentById(UIComponent base, String id) {
		if (id.equals(base.getId())) {
			return base;
		}
		
		UIComponent kid = null;
		UIComponent result = null;
		Iterator<UIComponent> kids = base.getFacetsAndChildren();
		while (kids.hasNext()) {
			kid = kids.next();
			if (id.equals(kid.getId())) {
				result = kid;
				break;
			}
			result = findComponentById(kid, id);
			if (result != null) {
				break;
			}
		}
		
		return result;
	}

	private static List<UIComponent> findComponentsByBinding(UIComponent base, String binding) {
		List<UIComponent> result = new ArrayList<>();
		findComponentsByBinding(base, binding, result);

		// check the last part of a compound binding
		// eg compound.binding
		if (result.isEmpty()) {
			int lastDotIndex = binding.lastIndexOf('.');
			if (lastDotIndex > 0) { // compound binding
				// This is the simple binding prepended with a dot (in case we have some compound bindings in the UI)
				String dotSimpleBinding = binding.substring(lastDotIndex);
				findComponentsByBinding(base, dotSimpleBinding, result);
			}
		}
		
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
