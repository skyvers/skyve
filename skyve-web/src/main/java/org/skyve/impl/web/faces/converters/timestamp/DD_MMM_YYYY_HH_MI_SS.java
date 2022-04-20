package org.skyve.impl.web.faces.converters.timestamp;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.UtilImpl;

public class DD_MMM_YYYY_HH_MI_SS extends org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
    	String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return fromDisplayValue(value);
			}
			catch (ConversionException e) {
				String message = e.getMessages().get(0).getText();
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR, message, message), e);
			}
    	}
    	return null;
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		if (value == null) {
			return "";
		}
		try {
			return toDisplayValue((Timestamp) value);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "";
		}
	}
}
