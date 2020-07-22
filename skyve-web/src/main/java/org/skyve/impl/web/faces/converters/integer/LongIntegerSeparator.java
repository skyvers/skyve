package org.skyve.impl.web.faces.converters.integer;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.impl.util.UtilImpl;

public class LongIntegerSeparator extends org.skyve.domain.types.converters.integer.LongIntegerSeparator implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
    	String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return fromDisplayValue(processedValue);
			}
			catch (NumberFormatException nfe) {
				// check if the supplied value was a String which exceeded the max parseable int value
				if (value.matches("[+]?\\d{19,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Long max value (%s)",
							java.lang.Long.valueOf(java.lang.Long.MAX_VALUE));
					throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
							message, message), nfe);
				} else if (value.matches("[-]?\\d{19,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Long min value (%s)",
							java.lang.Long.valueOf(java.lang.Long.MIN_VALUE));
					throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
							message, message), nfe);
				}
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
						"Must be a whole number",
						"Must be a whole number"),
						nfe);
			}
			catch (Exception e) {
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
			return toDisplayValue((Long) value);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}
	}
}
