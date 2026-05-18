package org.skyve.impl.web.faces.converters.timestamp;

import org.junit.Assert;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.skyve.domain.types.Timestamp;
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
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		Assert.assertEquals(testDate, converter.getAsObject(null, null, "2020/03/01"));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		Assert.assertEquals("2020/03/01", converter.getAsString(null, null, testDate));
	}

	@Test
	public void testGetAsObjectNullValue() throws Exception {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	public void testGetAsStringNullValue() throws Exception {
		assertThat(converter.getAsString(null, null, null), is(""));
	}
}