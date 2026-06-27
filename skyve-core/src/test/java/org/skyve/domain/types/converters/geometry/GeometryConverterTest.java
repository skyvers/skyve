package org.skyve.domain.types.converters.geometry;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.messages.ConversionException;
import org.skyve.metadata.model.Attribute.AttributeType;

class GeometryConverterTest {

	@Test
	@SuppressWarnings("static-method")
	void getValueTypeIsGeometry() {
		GeometryConverter converter = new GeometryConverter();
		assertThat(converter.getValueType(), is(Geometry.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void getAttributeTypeIsGeometry() {
		GeometryConverter converter = new GeometryConverter();
		assertThat(converter.getAttributeType(), is(AttributeType.geometry));
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatIsNull() {
		GeometryConverter converter = new GeometryConverter();
		assertNull(converter.getFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	void getValidatorIsNull() {
		GeometryConverter converter = new GeometryConverter();
		assertNull(converter.getValidator());
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatPatternIsNull() {
		GeometryConverter converter = new GeometryConverter();
		assertNull(converter.getFormatPattern());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueParsesWKT() throws ConversionException {
		GeometryConverter converter = new GeometryConverter();
		Geometry geom = converter.fromDisplayValue("POINT (1 2)");
		assertNotNull(geom);
		assertThat(geom.getGeometryType(), is("Point"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayValueWritesWKT() throws ConversionException {
		GeometryConverter converter = new GeometryConverter();
		Geometry geom = converter.fromDisplayValue("POINT (1 2)");
		String wkt = converter.toDisplayValue(geom);
		assertThat(wkt, containsString("POINT"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueInvalidWKTThrowsConversionException() {
		GeometryConverter converter = new GeometryConverter();
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("NOT VALID WKT!!!"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayValueNullThrowsConversionException() {
		GeometryConverter converter = new GeometryConverter();
		assertThrows(ConversionException.class, () -> converter.toDisplayValue(null));
	}
}
