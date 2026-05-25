package org.skyve.impl.domain.types.jaxb;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.domain.messages.DomainException;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlSchemaType(name = "string")
//@XmlJavaTypeAdapter(GeometryMapper.class)
/**
 * JAXB {@link XmlAdapter} that converts between a Well-Known Text (WKT) string
 * and a JTS {@link Geometry} object.
 *
 * <p>On unmarshalling, parses the WKT string using a {@link org.locationtech.jts.io.WKTReader}.
 * On marshalling, converts the geometry to its canonical WKT representation via
 * {@code Geometry.toText()}.  A {@code null} input produces a {@code null} output
 * in both directions.
 *
 * @throws org.locationtech.jts.io.ParseException if the WKT string is malformed
 *
 * <p>Threading: not thread-safe; a new {@link org.locationtech.jts.io.WKTReader}
 * is created on each unmarshal call (WKTReader is not reentrant).
 *
 * @see Geometry
 */
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
