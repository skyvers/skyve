package org.skyve.impl.web.faces.converters.lang;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.impl.util.UtilImpl;

/**
 * Convert an Integer to and from a string respecting null values.
 * @author mike
 */
public class Boolean implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			return java.lang.Boolean.valueOf(processedValue);
		}

		return null;
    }

    @Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, Object value) {
        if (value instanceof java.lang.Boolean) {
            return value.toString();
        }
        
        return null;
    }
}
