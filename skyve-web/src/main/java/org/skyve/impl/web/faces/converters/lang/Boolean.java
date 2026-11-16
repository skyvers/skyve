package org.skyve.impl.web.faces.converters.lang;

import org.skyve.impl.util.UtilImpl;

import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

/**
 * Convert an Integer to and from a string respecting null values.
 * @author mike
 */
public class Boolean implements Converter<java.lang.Boolean> {
	/**
	 * Parses a UI string value into the corresponding Skyve domain value for this converter format.
	 *
	 * @param fc the active JSF context used to resolve locale and conversion error handling
	 * @param component the component requesting conversion
	 * @param value the submitted UI value
	 * @return the converted domain value, or {@code null} when the submitted value is blank
	 * @throws ConverterException when the submitted value cannot be parsed using this format
	 */
	@Override
	public @Nullable java.lang.Boolean getAsObject(@Nullable FacesContext context, @Nullable UIComponent component, @Nullable java.lang.String value) {
		java.lang.String processedValue = UtilImpl.processStringValue(value);
		if (processedValue != null) {
			return java.lang.Boolean.valueOf(processedValue);
		}

		return null;
	}

	/**
	 * Formats the domain boolean value for UI rendering.
	 */
	@Override
    public @Nullable java.lang.String getAsString(@Nullable FacesContext context, @Nullable UIComponent component, @Nullable java.lang.Boolean value) {
        return (value == null) ? null : value.toString();
    }
}
