package org.skyve.impl.web.faces.converters.integer;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.impl.util.UtilImpl;

public class IntegerSeparator extends org.skyve.domain.types.converters.integer.IntegerSeparator implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
		String processedValue = UtilImpl.processStringValue(value);
		if (processedValue != null) {
			try {
				return fromDisplayValue(processedValue);
			} catch (NumberFormatException nfe) {
				// check if the supplied value was a String which exceeded the max parseable int value
				if (value.matches("[+]?\\d{11,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Integer max value (%s)",
							java.lang.Integer.valueOf(java.lang.Integer.MAX_VALUE));
					throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
							message, message), nfe);
				} else if (value.matches("[-]?\\d{11,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Integer min value (%s)",
							java.lang.Integer.valueOf(java.lang.Integer.MIN_VALUE));
					throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
							message, message), nfe);
				}

				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
						"Must be a whole number",
						"Must be a whole number"),
						nfe);
			} catch (Exception e) {
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
						"Must be a whole number",
						"Must be a whole number"),
						e);
			}
		}
		return null;
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return toDisplayValue((Integer) value);
		} catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}
	}
}
