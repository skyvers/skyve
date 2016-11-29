package org.skyve.impl.web.faces;

import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.Map;

import javax.faces.FacesException;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.context.FacesContext;

public abstract class FacesAction<T> {
	public final T execute() {
		T result = null;
		
		try {
			try {
				result = callback();
			}
			catch (InvocationTargetException e) {
				throw e.getTargetException();
			}
		}
		catch (Throwable t) {
			throw new FacesException(t);
		}
		
		return result;
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

	public static UIComponent findComponentByClientId(UIComponent base, String id) {
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
}
