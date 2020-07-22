package org.skyve.impl.web.faces.converters.lang;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.impl.util.UtilImpl;

/**
 * Convert an Integer to and from a String respecting null values.
 * 
 * @author mike
 */
public class Integer implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
	        try {
	            return java.lang.Integer.valueOf(processedValue);
	        }
	        catch (NumberFormatException e) {
				// check if the supplied value was a String which exceeded the max parseable int value
				if (value.matches("[+]?\\d{11,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Integer max value (%s)",
							java.lang.Integer.valueOf(java.lang.Integer.MAX_VALUE));
					throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
							message, message), e);
				} else if (value.matches("[-]?\\d{11,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Integer min value (%s)",
							java.lang.Integer.valueOf(java.lang.Integer.MIN_VALUE));
					throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
							message, message), e);
				}

				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
						"Must be a whole number",
						"Must be a whole number"),
						e);
	        }
    	}
    	return null;
    }

    @Override
    public java.lang.String getAsString(FacesContext context, UIComponent component, Object value) {
        if (value instanceof java.lang.Integer) {
            return value.toString();
        }
        
        return null;
    }
}
