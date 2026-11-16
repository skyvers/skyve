package org.skyve.impl.web.faces.converters.geometry;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;

import jakarta.faces.convert.ConverterException;
class GeometryConverterTest {
	private GeometryConverter converter;

	@BeforeEach
	void before() {
		converter = new GeometryConverter();
	}

	@Test
	void testGetAsObjectNullValue() {
		assertThat(converter.getAsObject(null, null, null), nullValue());
	}

	@Test
	void testGetAsObjectEmptyValue() {
		assertThat(converter.getAsObject(null, null, ""), nullValue());
	}

	@Test
	void testGetAsObjectInvalidValue() {
		assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, "not a geometry"));
	}

	@SuppressWarnings("null") // a null here fails the test
	@Test
	void testGetAsObjectValidWkt() {
		Geometry result = converter.getAsObject(null, null, "POINT (1 2)");
		assertThat(result.getGeometryType(), is("Point"));
	}

	@Test
	void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	void testGetAsStringValidValue() {
		Geometry geometry = converter.getAsObject(null, null, "POINT (1 2)");
		String result = converter.getAsString(null, null, geometry);
		assertThat(result, is("POINT (1 2)"));
	}
}
