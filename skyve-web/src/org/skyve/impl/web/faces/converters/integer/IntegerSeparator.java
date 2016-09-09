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
    	java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return fromDisplayValue(processedValue);
			}
			catch (Exception e) {
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
																"Invalid number",
																"Invalid number"),
												e);
			}
		}
		return null;
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return toDisplayValue((Integer) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
