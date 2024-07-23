package org.skyve.impl.web.faces.converters.lang;

import org.skyve.impl.util.UtilImpl;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

/**
 * Convert a String to and from a string respecting null values.
 * @author mike
 */
public class String implements Converter<java.lang.String> {
    @Override
    public java.lang.String getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	return UtilImpl.processStringValue(value);
    }

    @Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, java.lang.String value) {
    	return value;
    }
}
