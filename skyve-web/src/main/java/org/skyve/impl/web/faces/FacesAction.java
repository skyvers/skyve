package org.skyve.impl.web.faces;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.el.ValueExpression;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.context.FacesContext;

import org.skyve.domain.messages.MessageException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.domain.messages.Message;

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
			persistence.rollback();
			t.printStackTrace();
			
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
	
	public abstract T callback() throws Exception;
	
    public static boolean validateRequiredFields() {
    	boolean result = true;
    	
    	FacesContext fc = FacesContext.getCurrentInstance();
    	Map<String, String> requestMap = fc.getExternalContext().getRequestParameterMap();
    	for (String paramName : requestMap.keySet()) {
    		String paramValue = requestMap.get(paramName);
    		String clientId = paramName;
    		clientId = clientId.replaceAll(":\\d+:", ":"); // remove the grid indexing used in request parameters
    		if (clientId.endsWith("_input")) { // date columns, check boxes etc
    			clientId = clientId.substring(0, clientId.length() - 6);
    		}
    		if ((paramValue == null) || "".equals(paramValue.trim())) {
    			UIComponent component = findComponentByClientId(fc.getViewRoot(), clientId);
    			if ((component != null) && component.isRendered() && (component instanceof UIInput)) {
					String message = ((UIInput) component).getRequiredMessage();
					if (message != null) {
						result = false;
						FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message);
    					fc.addMessage(component.getClientId(), msg);
    					fc.addMessage(null, msg);
					}
    			}
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

	private static UIComponent findComponentByClientId(UIComponent base, String id) {
		if (id.equals(base.getClientId())) {
			return base;
		}
		
		UIComponent kid = null;
		UIComponent result = null;
		Iterator<UIComponent> kids = base.getFacetsAndChildren();
		while (kids.hasNext()) {
			kid = kids.next();
			if (id.equals(kid.getClientId())) {
				result = kid;
				break;
			}
			result = findComponentByClientId(kid, id);
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
		int lastDotIndex = binding.lastIndexOf('.');
		if (lastDotIndex > 0) { // compound binding
			String simpleBinding = binding.substring(lastDotIndex);
			findComponentsByBinding(base, simpleBinding, result);
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
