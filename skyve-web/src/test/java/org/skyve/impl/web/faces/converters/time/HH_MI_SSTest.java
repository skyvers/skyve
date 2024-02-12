package org.skyve.impl.web.faces.converters.time;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.TimeOnly;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;

public class HH_MI_SSTest {
	private HH_MI_SS formatter;

	@Before
	public void before() {
		formatter = new HH_MI_SS();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		formatter.getAsObject(null, null, "03-01-2020 02:30:05");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormatAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 05);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "02:30:05 AM"), is(testDate));
	}

	@Test
	public void testGetAsObjectValidFormatPM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 05);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "02:30:05 PM"), is(testDate));
	}

	@Test
	public void testGetAsStringAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("02:30:55 AM"));
	}

	@Test
	public void testGetAsStringPM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("02:30:55 PM"));
	}
}
