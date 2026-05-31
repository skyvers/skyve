package org.skyve.impl.web.faces.converters.datetime;

import org.junit.Assert;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNull;
import org.junit.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
public class YYYY_MM_DDTest {
	private YYYY_MM_DD formatter = new YYYY_MM_DD();

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() {
		// call the method under test
		formatter.getAsObject(null, null, "2020-01-03");

		Assert.fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormat() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));

		// call the method under test
		Assert.assertEquals(testDate, formatter.getAsObject(null, null, "2020/03/01"));
	}

	@Test
	public void testGetAsString() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assert.assertEquals("2020/03/01", formatter.getAsString(null, null, testDate));
	}

	@Test
	public void testGetAsObjectNullValue() {
		assertNull(formatter.getAsObject(null, null, null));
	}

	@Test
	public void testGetAsStringNullValue() {
		assertThat(formatter.getAsString(null, null, null), is(""));
	}
}