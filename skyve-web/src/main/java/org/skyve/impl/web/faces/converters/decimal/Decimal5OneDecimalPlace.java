package org.skyve.impl.web.faces.converters.decimal;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.domain.types.Decimal5;
import org.skyve.impl.util.UtilImpl;

public class Decimal5OneDecimalPlace extends org.skyve.domain.types.converters.decimal.Decimal5OneDecimalPlace implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
    	String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return fromDisplayValue(processedValue);
			}
			catch (Exception e) {
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
																"Must be a decimal number",
																"Must be a decimal number"),
												e);
			}
		}
		return null;
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return toDisplayValue((Decimal5) value);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}
	}
}
