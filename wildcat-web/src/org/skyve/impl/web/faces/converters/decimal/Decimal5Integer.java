package org.skyve.impl.web.faces.converters.decimal;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.domain.types.Decimal5;
import org.skyve.impl.util.UtilImpl;

public class Decimal5Integer extends org.skyve.domain.types.converters.decimal.Decimal5Integer implements Converter {
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
			return toDisplayValue((Decimal5) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
