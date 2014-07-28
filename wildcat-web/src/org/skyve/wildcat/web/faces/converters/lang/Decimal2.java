package org.skyve.wildcat.web.faces.converters.lang;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.wildcat.util.UtilImpl;

/**
 * Convert a Decimal2 to and from a string respecting null values.
 * @author mike
 */
public class Decimal2 implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
	    	try {
	            return new org.skyve.domain.types.Decimal2(processedValue);
	        }
	        catch (NumberFormatException e) {
	            return null;
	        }
    	}
    	return null;
    }

    @Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, Object value) {
        if (value instanceof org.skyve.domain.types.Decimal2) {
            return value.toString();
        }
        
        return null;
    }
}
