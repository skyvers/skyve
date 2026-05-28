package org.skyve.impl.web.faces.converters.datetime;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.util.UtilImpl;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;
import jakarta.faces.convert.ConverterException;

/**
 * Converts JSF values between formatted UI strings and Skyve domain representations for this format.
 */
public class DD_MMM_YYYY extends org.skyve.domain.types.converters.datetime.DD_MMM_YYYY
							implements Converter<DateTime> {
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
	public DateTime getAsObject(FacesContext fc, UIComponent component, String value) {
    	String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return fromDisplayValue(processedValue);
			}
			catch (ConversionException e) {
				String message = e.getMessages().get(0).getText();
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message), e);
			}
		}
		return null;
	}

	/**
	 * Formats a Skyve domain value to the display string expected by this converter format.
	 *
	 * @param fc the active JSF context used to resolve locale-specific formatting
	 * @param component the component requesting conversion
	 * @param value the domain value to format
	 * @return a formatted display value, or an empty string when the value is {@code null} or formatting fails
	 */
	@Override
	public String getAsString(FacesContext fc, UIComponent component, DateTime value) {
		if (value == null) {
			return "";
		}
		try {
			return toDisplayValue(value);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "";
		}
	}
}
