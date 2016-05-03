package org.skyve.impl.web.faces.converters.geometry;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.skyve.impl.util.UtilImpl;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jts.io.WKTWriter;

public class GeometryConverter implements Converter {
	@Override
	public Object getAsObject(FacesContext fc, UIComponent component, String value) {
		java.lang.String processedValue = UtilImpl.processStringValue(value);
    	if (processedValue != null) {
			try {
				return new WKTReader().read(value);
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
			return new WKTWriter().write((Geometry) value);
		}
		catch (Exception e) {
			return null;
		}
	}
}
