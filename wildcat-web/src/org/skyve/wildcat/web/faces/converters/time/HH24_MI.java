package org.skyve.wildcat.web.faces.converters.time;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.domain.types.TimeOnly;

public class HH24_MI extends org.skyve.domain.types.converters.time.HH24_MI implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
		try {
			return fromDisplayValue(value);
		}
		catch (Exception e) {
			return null;
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
