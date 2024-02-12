package org.skyve.impl.web.faces;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.primefaces.component.datatable.DataTable;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

import jakarta.el.ValueExpression;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.application.FacesMessage.Severity;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIData;
import jakarta.faces.component.UIInput;
import jakarta.faces.component.visit.VisitCallback;
import jakarta.faces.component.visit.VisitContext;
import jakarta.faces.component.visit.VisitResult;
import jakarta.faces.context.FacesContext;

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
		// Allow security problems out so they are redirected to the error page
		catch (SecurityException e) {
			persistence.setRollbackOnly();
			throw e;
		}
		catch (Throwable t) {
			persistence.setRollbackOnly();
			t.printStackTrace();
			
			if (t instanceof MessageException) {
				TreeSet<String> globalMessageSet = new TreeSet<>();
				for (Message em : ((MessageException) t).getMessages()) {
					processFacesMessages(fc, FacesMessage.SEVERITY_ERROR, em, globalMessageSet);
				}

				// Render only field level error messages
				Collection<String> renderIds = fc.getPartialViewContext().getRenderIds();
				renderIds.clear();
				List<String> messageClientIds = new ArrayList<>();
				findAllMessageComponentClientIds(fc.getViewRoot(), messageClientIds);
				renderIds.addAll(messageClientIds);
			}
			else {
				FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR, t.getMessage(), t.getMessage());
		        fc.addMessage(null, msg);
		        // render nothing here since we are only adding a global message
		        fc.getPartialViewContext().getRenderIds().clear();
			}
		}
		
		return result;
	}

	/**
	 * If the message is bound, add it globally and add it to all it's bindings.
	 * If the message is not bound, just add it globally.
	 * The approach relies on the template.xhtmls having something like...
	 * 
	 * <p:messages id="messages" globalOnly="true">
	 * 		<p:autoUpdate />
	 * </p:messages>
	 * <p:growl id="growl" globalOnly="true" showDetail="false">
	 * 		<p:autoUpdate />
	 * </p:growl>
	 * 
	 * and
	 * 
	 * <p:message for="<binding>" on each input widget.
	 * Adding messages manually for each type of message display mechanism affords the most flexibility.
	 * 
	 * @param context
	 * @param severity
	 * @param message	
	 * @param globalMessageSet	Used to add only unique/distinct messages across calls
	 */
	public static void processFacesMessages(FacesContext context,
												Severity severity,
												Message message,
												Set<String> globalMessageSet) {
		String text = message.getText();
		FacesMessage msg = new FacesMessage(severity, text, text);
		for (String binding : message.getBindings()) {
			List<UIComponent> components = findComponentsByBinding(context.getViewRoot(), binding);
			for (UIComponent component : components) {
				if (component.isRendered()) {
					context.addMessage(component.getClientId(), msg);
				}
			}

			// find components if this is an indexed compound binding
			// eg dataWidgetBinding[\d*].simpleBinding or dataWidgetBinding[\d*].compound.binding etc
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
							if (lastColonIndex > 0) { // it really is a row in a grid
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
		}

		// only add distinct error messages globally
		if (globalMessageSet.add(text)) {
			context.addMessage(null, msg);
		}
	}
	
	public abstract T callback() throws Exception;
	
    public static boolean validateRequiredFields() {
    	TreeSet<String> globalMessages = new TreeSet<>(); // used for removing duplicate global messages
    	FacesContext fc = FacesContext.getCurrentInstance();
    	boolean valid = validateRequiredFields(fc, fc.getViewRoot(), globalMessages);

    	// If we failed validation, set the renderIds collected for all field messages
    	if (! valid) {
    		Collection<String> renderIds = fc.getPartialViewContext().getRenderIds();
    		renderIds.clear();
    		List<String> messageClientIds = new ArrayList<>();
    		findAllMessageComponentClientIds(fc.getViewRoot(), messageClientIds);
    		renderIds.addAll(messageClientIds);
    	}
    	
    	return valid;
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

	private static boolean checkRequiredInput(FacesContext fc,
												UIInput input,
												TreeSet<String> globalMessages) {
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
	
	private static void findAllMessageComponentClientIds(UIComponent component, List<String> messageClientIds) {
		if (component.isRendered()) {
			if (component instanceof org.primefaces.component.message.Message) {
				// Determine if this Message is in a data grid
				String clientId = component.getClientId();
				int gridSize = 0;
				int lastColonIndex = clientId.lastIndexOf(':'); // positive if namespaced
				if (lastColonIndex > 0) { // namespaced clientId - could be a datagrid parent
					UIComponent parent = component.getParent();
					while (parent != null) {
						// Although PF DataTables implement both listGrids and dataGrids in PF,
						// only dataGrids have Message components in their columns.
						if (parent instanceof DataTable) {
							// Work out how many rows are in the data grid and set update strings appropriately
							List<?> value = (List<?>) ((DataTable) parent).getValue();
							if (value != null) {
								gridSize = value.size();
							}
							else {
				            	UtilImpl.LOGGER.warning("FacesAction.findAllMessageComponentClientIds: ClientID " + clientId + " points to a data grid that has a data table value of null");
							}
							break;
						}
						parent = parent.getParent();
					}
				}
				if (gridSize > 0) {
					for (int i = 0; i < gridSize; i++) {
						StringBuilder sb = new StringBuilder(clientId);
						sb.insert(lastColonIndex, i);
						sb.insert(lastColonIndex, ':');
						messageClientIds.add(sb.toString());
					}
				}
				else {
					messageClientIds.add(clientId);
				}
			}
			Iterator<UIComponent> kids = component.getFacetsAndChildren();
			while (kids.hasNext()) {
				findAllMessageComponentClientIds(kids.next(), messageClientIds);
			}
		}
	}
}
