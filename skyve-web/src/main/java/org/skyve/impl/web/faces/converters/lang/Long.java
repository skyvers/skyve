package org.skyve.impl.web.faces.converters.lang;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.skyve.impl.util.UtilImpl;

/**
 * Convert a Long to and from a String respecting null values.
 * 
 * @author mike
 */
public class Long implements Converter {
    @Override
    public Object getAsObject(FacesContext context, UIComponent component, java.lang.String value) {
    	java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
	    	try {
	            return java.lang.Long.valueOf(processedValue);
	        }
	        catch (NumberFormatException e) {
				// check if the supplied value was a String which exceeded the max parseable int value
				if (value.matches("[+]?\\d{19,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Long max value (%s)",
							java.lang.Long.valueOf(java.lang.Long.MAX_VALUE));
					throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
							message, message), e);
				} else if (value.matches("[-]?\\d{19,}")) {
					java.lang.String message = java.lang.String.format("Value exceeds Long min value (%s)",
							java.lang.Long.valueOf(java.lang.Long.MIN_VALUE));
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
        if (value instanceof java.lang.Long) {
            return value.toString();
        }
        
        return null;
    }
}
