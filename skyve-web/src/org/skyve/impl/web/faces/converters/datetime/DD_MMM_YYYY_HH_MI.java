package org.skyve.impl.web.faces.converters.datetime;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.domain.types.DateTime;
import org.skyve.impl.util.UtilImpl;

public class DD_MMM_YYYY_HH_MI extends org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
    	java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return fromDisplayValue(processedValue);
			}
			catch (Exception e) {
				return null;
			}
		}
		return null;
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return toDisplayValue((DateTime) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
