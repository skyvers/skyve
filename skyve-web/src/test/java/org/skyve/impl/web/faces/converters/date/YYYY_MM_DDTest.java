package org.skyve.impl.web.faces.converters.date;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;

public class YYYY_MM_DDTest {
	private YYYY_MM_DD converter = new YYYY_MM_DD();

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		converter.getAsObject(null, null, "2020-03-01");

		Assert.fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormat() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assert.assertEquals(testDate, converter.getAsObject(null, null, "2020/03/01"));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assert.assertEquals("2020/03/01", converter.getAsString(null, null, testDate));
	}
}
