package org.skyve.impl.web.faces.converters.lang;

import org.skyve.impl.util.UtilImpl;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

/**
 * Convert an Integer to and from a string respecting null values.
 * @author mike
 */
public class Boolean implements Converter<java.lang.Boolean> {
	@Override
	public java.lang.Boolean getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
		java.lang.String processedValue = UtilImpl.processStringValue(value);
		if (processedValue != null) {
			return java.lang.Boolean.valueOf(processedValue);
		}

		return null;
	}

	@Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, java.lang.Boolean value) {
        return (value == null) ? null : value.toString();
    }
}
