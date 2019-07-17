package org.skyve.impl.web.faces.converters.geometry;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.impl.util.UtilImpl;

public class GeometryConverter implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
		String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return new WKTReader().read(value);
			}
			catch (Exception e) {
				throw new ConverterException(new FacesMessage(FacesMessage.SEVERITY_ERROR,
																"Invalid geometry (use WKT format)",
																"Invalid geometry (use WKT format)"),
												e);
			}
		}
		return null;
	}

	@Override
	public String getAsString(FacesContext fc, UIComponent component, Object value) {
		try {
			return new WKTWriter().write((Geometry) value);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}
	}
}
