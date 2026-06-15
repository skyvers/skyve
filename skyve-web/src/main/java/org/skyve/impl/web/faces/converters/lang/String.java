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
    /**
     * Converts an incoming UI string value to the model representation.
     *
     * @param context the current Faces context
     * @param component the component requesting conversion
     * @param value the raw submitted value
     * @return the normalised string value
     */
    @Override
    public java.lang.String getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	return UtilImpl.processStringValue(value);
    }

    /**
     * Converts a model string value to the UI representation.
     *
     * @param context the current Faces context
     * @param component the component requesting conversion
     * @param value the model value
     * @return the rendered string value
     */
    @Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, java.lang.String value) {
    	return value;
    }
}
