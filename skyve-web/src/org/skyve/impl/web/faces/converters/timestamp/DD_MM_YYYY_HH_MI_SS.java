package org.skyve.impl.web.faces.converters.timestamp;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.domain.types.Timestamp;

public class DD_MM_YYYY_HH_MI_SS extends org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH_MI_SS implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
		try {
			return fromDisplayValue(value);
		}
		catch (Exception e) {
			throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
															"Invalid date/time (use DD-MM-YYYY HH:MI:SS format)",
															"Invalid date/time (use DD-MM-YYYY HH:MI:SS format)"),
											e);
		}
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return toDisplayValue((Timestamp) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
