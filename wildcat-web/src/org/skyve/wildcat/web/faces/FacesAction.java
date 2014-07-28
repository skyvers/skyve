package org.skyve.wildcat.web.faces;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.el.ValueExpression;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.skyve.domain.messages.ErrorMessage;
import org.skyve.wildcat.persistence.AbstractPersistence;

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
			
			if (t instanceof ErrorMessage) {
				ErrorMessage em = (ErrorMessage) t;
				processErrors(fc, em);
			}
			else {
				FacesMessage msg = new FacesMessage(t.getMessage(), t.getMessage());
		        fc.addMessage(null, msg);
			}
		}
		
		return result;
	}

	private static void processErrors(FacesContext context, ErrorMessage em) {
		boolean bound = false;
		String message = em.getErrorMessage();
		FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message);
		for (String binding : em.getBindings()) {
			List<UIComponent> components = findComponentsByBinding(context.getViewRoot(), binding);
			for (UIComponent component : components) {
				if (component.isRendered()) {
					context.addMessage(component.getClientId(), msg);
					bound = true;
				}
			}
		}
		if (! bound) {
			context.addMessage(null, msg);
		}
		List<ErrorMessage> subs = em.getSubordinates();
		if (subs != null) {
			for (ErrorMessage sub : subs) {
				processErrors(context, sub);
			}
		}
	}
	
	public abstract T callback() throws Exception;
	
	public static UIComponent findComponentById(UIComponent base, String id) {
	    if (id.equals(base.getId()))
	      return base;
	  
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

	public static List<UIComponent> findComponentsByBinding(UIComponent base, String binding) {
		List<UIComponent> result = new ArrayList<>();
		findComponentsByBinding(base, binding, result);
		return result;
	}

	private static void findComponentsByBinding(UIComponent base, String binding, List<UIComponent> result) {
		ValueExpression ve = base.getValueExpression("value");
		if (ve != null) {
			String expression = ve.getExpressionString();
			int bindingIndex = expression.indexOf(binding);
			// found a match if binding is at the end of the expression (minus the last '}')
			if ((bindingIndex >= 0) && (bindingIndex + binding.length() == expression.length() - 1)) {
				result.add(base);
			}
		}

		Iterator<UIComponent> kids = base.getFacetsAndChildren();
		while (kids.hasNext()) {
			findComponentsByBinding(kids.next(), binding, result);
		}
	}
}
