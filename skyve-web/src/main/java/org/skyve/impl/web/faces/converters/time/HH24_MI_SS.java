package org.skyve.impl.web.faces.converters.time;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.impl.util.UtilImpl;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;
import jakarta.faces.convert.ConverterException;

public class HH24_MI_SS extends org.skyve.domain.types.converters.time.HH24_MI_SS
							implements Converter<TimeOnly> {
	@Override
	public TimeOnly getAsObject(FacesContext fc, UIComponent component, String value) {
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

	@Override
	public String getAsString(FacesContext fc, UIComponent component, TimeOnly value) {
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
