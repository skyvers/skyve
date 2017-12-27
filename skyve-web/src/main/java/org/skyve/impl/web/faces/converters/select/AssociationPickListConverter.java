package org.skyve.impl.web.faces.converters.select;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.primefaces.component.picklist.PickList;
import org.primefaces.model.DualListModel;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

public class AssociationPickListConverter implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, String value) {
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

    @Override
    public String getAsString(FacesContext context, UIComponent component, Object value) {
    	return (value == null) ? null : ((DomainValue) value).getCode();
    }
}
