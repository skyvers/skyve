package org.skyve.impl.web.faces.converters.decimal.currency;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.domain.types.Decimal2;
import org.skyve.impl.util.UtilImpl;

public class Decimal2DollarsAndCents extends org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents implements Converter {
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
		return  null;
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return toDisplayValue((Decimal2) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
