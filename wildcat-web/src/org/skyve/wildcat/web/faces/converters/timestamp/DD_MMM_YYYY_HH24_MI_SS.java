package org.skyve.wildcat.web.faces.converters.timestamp;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.domain.types.Timestamp;

public class DD_MMM_YYYY_HH24_MI_SS extends org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS implements Converter {
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
			return toDisplayValue((Timestamp) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
