package org.skyve.impl.web.faces.converters.datetime;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;

public class YYYY_MM_DD_HH24_MITest {
	private YYYY_MM_DD_HH24_MI formatter = new YYYY_MM_DD_HH24_MI();

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		formatter.getAsObject(null, null, "2020-01-03 02:30");

		Assert.fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormatAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assert.assertEquals(testDate, formatter.getAsObject(null, null, "2020/03/01 02:30"));
	}

	@Test
	public void testGetAsObjectValidFormatPM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assert.assertEquals(testDate, formatter.getAsObject(null, null, "2020/03/01 14:30"));
	}

	@Test
	public void testGetAsStringAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assert.assertEquals("2020/03/01 02:30", formatter.getAsString(null, null, testDate));
	}

	@Test
	public void testGetAsStringPM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assert.assertEquals("2020/03/01 14:30", formatter.getAsString(null, null, testDate));
	}
}
