package org.skyve.domain.types.converters.datetime;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

public class YYYY_MM_DDTest {
	private YYYY_MM_DD converter = new YYYY_MM_DD();

	@Test(expected = ConversionException.class)
	public void testFromDisplayValueInvalidFormat() throws Exception {
		// call the method under test
		converter.fromDisplayValue("01-03-2020");

		Assert.fail("ConversionException should be thrown");
	}

	@Test
	public void testFromDisplayValue() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));

		// call the method under test
		Assert.assertEquals(testDate, converter.fromDisplayValue("2020/03/01"));
	}
	@Test
	public void testToDisplayValue() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assert.assertEquals("2020/03/01", converter.toDisplayValue(testDate));
	}
}
