package org.skyve.impl.web.faces.converters.timestamp;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;

public class YYYY_MM_DD_HH24_MI_SSTest {
	private YYYY_MM_DD_HH24_MI_SS converter = new YYYY_MM_DD_HH24_MI_SS();

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		converter.getAsObject(null, null, "2020-03-01 02:30:05");

		Assert.fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormatAM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 05);

		// call the method under test
		Assert.assertEquals(testDate, converter.getAsObject(null, null, "2020/03/01 02:30:05"));
	}

	@Test
	public void testGetAsObjectValidFormatPM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 05);

		// call the method under test
		Assert.assertEquals(testDate, converter.getAsObject(null, null, "2020/03/01 14:30:05"));
	}

	@Test
	public void testGetAsStringAM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		Assert.assertEquals("2020/03/01 02:30:55", converter.getAsString(null, null, testDate));
	}

	@Test
	public void testGetAsStringPM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		Assert.assertEquals("2020/03/01 14:30:55", converter.getAsString(null, null, testDate));
	}
}
