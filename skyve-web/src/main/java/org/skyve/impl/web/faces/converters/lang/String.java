package org.skyve.impl.web.faces.converters.lang;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.impl.util.UtilImpl;

/**
 * Convert a String to and from a string respecting null values.
 * @author mike
 */
public class String implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	return UtilImpl.processStringValue(value);
    }

    @Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, Object value) {
        if (value instanceof java.lang.String) {
            return (java.lang.String) value;
        }
        
        return null;
    }
}
