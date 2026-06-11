package org.skyve.impl.web.faces.converters.geometry;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.locationtech.jts.geom.Geometry;

import jakarta.faces.convert.ConverterException;
public class GeometryConverterTest {
	private GeometryConverter converter;

	@Before
	public void before() {
		converter = new GeometryConverter();
	}

	@Test
	public void testGetAsObjectNullValue() {
		assertThat(converter.getAsObject(null, null, null), nullValue());
	}

	@Test
	public void testGetAsObjectEmptyValue() {
		assertThat(converter.getAsObject(null, null, ""), nullValue());
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidValue() {
		converter.getAsObject(null, null, "not a geometry");
		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidWkt() {
		Geometry result = converter.getAsObject(null, null, "POINT (1 2)");
		assertThat(result.getGeometryType(), is("Point"));
	}

	@Test
	public void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	public void testGetAsStringValidValue() {
		Geometry geometry = converter.getAsObject(null, null, "POINT (1 2)");
		String result = converter.getAsString(null, null, geometry);
		assertThat(result, is("POINT (1 2)"));
	}
}
