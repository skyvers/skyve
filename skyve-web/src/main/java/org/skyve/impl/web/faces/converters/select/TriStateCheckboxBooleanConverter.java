package org.skyve.impl.web.faces.converters.select;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

/**
 * Converts JSF values between formatted UI strings and Skyve domain representations for this format.
 */
public class TriStateCheckboxBooleanConverter implements Converter<Boolean> {
	/**
	 * Parses tri-state checkbox values from the UI into nullable booleans.
	 *
	 * @param context the active JSF context
	 * @param component the component requesting conversion
	 * @param value the submitted UI value
	 * @return the resolved domain value or identifier object
	 */
	@Override
	public Boolean getAsObject(FacesContext context, UIComponent component, String value) {
		if ("1".equals(value)) {
			return Boolean.TRUE;
		}
		else if ("2".equals(value)) {
			return Boolean.FALSE;
		}

		return null;
	}

	/**
	 * Formats nullable booleans to the tri-state checkbox string representation.
	 *
	 * @param context the active JSF context
	 * @param component the component requesting conversion
	 * @param value the domain value to convert
	 * @return the display or identifier string for the supplied value
	 */
	@Override
	public String getAsString(FacesContext context, UIComponent component, Boolean value) {
		if (Boolean.TRUE.equals(value)) {
			return "1";
		}
		else if (Boolean.FALSE.equals(value)) {
			return "2";
		}
		
		return "0";
	}
}
