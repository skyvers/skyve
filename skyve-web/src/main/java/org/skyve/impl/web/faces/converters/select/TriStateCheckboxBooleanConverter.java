package org.skyve.impl.web.faces.converters.select;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;

public class TriStateCheckboxBooleanConverter implements Converter<Boolean> {
	@Override
	public Boolean getAsObject(FacesContext context, UIComponent component, String value) {
		if ("1".equals(value)) {
			return Boolean.TRUE;
		}
		else if ("2".equals(value)) {
			return Boolean.FALSE;
		}

		return null;
	}

	@Override
	public String getAsString(FacesContext context, UIComponent component, Boolean value) {
		if (Boolean.TRUE.equals(value)) {
			return "1";
		}
		else if (Boolean.FALSE.equals(value)) {
			return "2";
		}
		
		return "0";
	}
}
