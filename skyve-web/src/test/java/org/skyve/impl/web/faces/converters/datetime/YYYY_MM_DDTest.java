package org.skyve.impl.web.faces.converters.datetime;

import javax.faces.convert.ConverterException;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

public class YYYY_MM_DDTest {
	private YYYY_MM_DD formatter = new YYYY_MM_DD();

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		formatter.getAsObject(null, null, "2020-01-03");

		Assert.fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormat() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));

		// call the method under test
		Assert.assertEquals(testDate, formatter.getAsObject(null, null, "2020/03/01"));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assert.assertEquals("2020/03/01", formatter.getAsString(null, null, testDate));
	}
}
