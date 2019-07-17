package org.skyve.impl.domain.types.jaxb;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.domain.messages.DomainException;

//@XmlSchemaType(name = "string")
//@XmlJavaTypeAdapter(GeometryMapper.class)
public class GeometryMapper extends XmlAdapter<String, Geometry> {
	@Override
	public Geometry unmarshal(String geometry) throws Exception {
		Geometry result = null;
		
		if (geometry != null) {
			try {
				result = new WKTReader().read(geometry);
			}
			catch (ParseException e) {
				throw new DomainException("Exception parsing " + geometry, e);
			}
		}

		return result;
	}

	@Override
	public String marshal(Geometry geometry) throws Exception {
		String result = null;
		
		if (geometry != null) {
			result = new WKTWriter().write(geometry);
		}

		return result;
	}
}
