package org.skyve.impl.web.faces.converters.time;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.domain.types.TimeOnly;

public class HH_MI extends org.skyve.domain.types.converters.time.HH_MI implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
		try {
			return fromDisplayValue(value);
		}
		catch (Exception e) {
			throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
															"Invalid time (use HH:MI format)",
															"Invalid time (use HH:MI format)"),
											e);
		}
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return toDisplayValue((TimeOnly) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
