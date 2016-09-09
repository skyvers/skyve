package org.skyve.impl.web.faces.converters.lang;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.impl.util.UtilImpl;

/**
 * Convert a Decimal10 to and from a string respecting null values.
 * @author mike
 */
public class Decimal10 implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
	        try {
	            return new org.skyve.domain.types.Decimal10(processedValue);
	        }
	        catch (NumberFormatException e) {
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
																"Invalid number",
																"Invalid number"),
												e);
	        }
        }
        return null;
    }

    @Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, Object value) {
        if (value instanceof org.skyve.domain.types.Decimal10) {
            return value.toString();
        }
        
        return null;
    }
}
