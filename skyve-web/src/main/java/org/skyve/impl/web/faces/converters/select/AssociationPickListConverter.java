package org.skyve.impl.web.faces.converters.select;

import org.primefaces.component.picklist.PickList;
import org.primefaces.model.DualListModel;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

/**
 * Converts JSF values between formatted UI strings and Skyve domain representations for this format.
 */
public class AssociationPickListConverter implements Converter<DomainValue> {
	/**
	 * Resolves a selected pick-list code back to a {@link DomainValue}.
	 *
	 * @param context the active JSF context
	 * @param component the pick-list component requesting conversion
	 * @param value the submitted pick-list code
	 * @return the matching domain value, or {@code null} when no match is found
	 */
    @Override
    public DomainValue getAsObject(FacesContext context, UIComponent component, String value) {
		DomainValue result = null;

		final String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
	    	@SuppressWarnings("unchecked")
			DualListModel<DomainValue> model = (DualListModel<DomainValue>) ((PickList) component).getValue();
	    	// Check source values
	    	for (DomainValue domainValue : model.getSource()) {
	    		if (domainValue.getCode().equals(processedValue)) {
	    			result = domainValue;
	    			break;
	    		}
	    	}
	    	// If not a source value, check target values
	    	if (result == null) {
		    	for (DomainValue domainValue : model.getTarget()) {
		    		if (domainValue.getCode().equals(processedValue)) {
		    			result = domainValue;
		    			break;
		    		}
		    	}
	    	}
    	}

    	return result;
    }

	/**
	 * Formats a {@link DomainValue} using its code for pick-list submission.
	 *
	 * @param context the active JSF context
	 * @param component the pick-list component requesting conversion
	 * @param value the domain value to encode
	 * @return the domain value code, or an empty string when the value is {@code null}
	 */
    @Override
    public String getAsString(FacesContext context, UIComponent component, DomainValue value) {
    	return (value == null) ? "" : value.getCode();
    }
}
