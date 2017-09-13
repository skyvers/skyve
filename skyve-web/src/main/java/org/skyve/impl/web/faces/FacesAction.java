package org.skyve.impl.web.faces;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import javax.el.ValueExpression;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.context.FacesContext;

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
    
	private static boolean validateRequiredFields(FacesContext fc, UIComponent component, TreeSet<String> globalMessages) {
		boolean result = true;
		
		// only process this component (and it's children) if it was rendered
		if (component.isRendered()) {
			if (component instanceof UIInput) {
				UIInput input = (UIInput) component;
				String message = input.getRequiredMessage();
				if (Util.processStringValue(message) != null) {
					Object value = input.getValue();
					if ((value == null) || ((value instanceof String) && ((String) value).trim().isEmpty())) {
						result = false;
						FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message);
						fc.addMessage(component.getClientId(), msg);
						
						// Add distinct global messages
						if (globalMessages.add(message)) {
							fc.addMessage(null, msg);
						}
					}
				}
			}
			
			Iterator<UIComponent> kids = component.getFacetsAndChildren();
			while (kids.hasNext()) {
				// NB ensure && operator doesn't short circuit by putting the function call as the first argument
				// This way we'll get all validation error messages
				result = validateRequiredFields(fc, kids.next(), globalMessages) && result;
			}
		}

		return result;
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
		if (result.isEmpty()) {
			int lastDotIndex = binding.lastIndexOf('.');
			if (lastDotIndex > 0) { // compound binding
				String simpleBinding = binding.substring(lastDotIndex);
				findComponentsByBinding(base, simpleBinding, result);
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
