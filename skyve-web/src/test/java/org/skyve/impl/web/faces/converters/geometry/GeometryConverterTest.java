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
	public void testGetAsObjectNullValue() throws Exception {
		assertThat(converter.getAsObject(null, null, null), nullValue());
	}

	@Test
	public void testGetAsObjectEmptyValue() throws Exception {
		assertThat(converter.getAsObject(null, null, ""), nullValue());
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidValue() throws Exception {
		converter.getAsObject(null, null, "not a geometry");
		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidWkt() throws Exception {
		Geometry result = converter.getAsObject(null, null, "POINT (1 2)");
		assertThat(result.getGeometryType(), is("Point"));
	}

	@Test
	public void testGetAsStringNullValue() throws Exception {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	public void testGetAsStringValidValue() throws Exception {
		Geometry geometry = converter.getAsObject(null, null, "POINT (1 2)");
		String result = converter.getAsString(null, null, geometry);
		assertThat(result, is("POINT (1 2)"));
	}
}
