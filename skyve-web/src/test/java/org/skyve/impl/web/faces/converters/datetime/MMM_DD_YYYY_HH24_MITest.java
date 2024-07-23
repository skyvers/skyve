package org.skyve.impl.web.faces.converters.datetime;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;

public class MMM_DD_YYYY_HH24_MITest {
	private MMM_DD_YYYY_HH24_MI formatter;

	@Before
	public void before() {
		formatter = new MMM_DD_YYYY_HH24_MI();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		formatter.getAsObject(null, null, "03-01-2020 02:30");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormatAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "Mar-01-2020 02:30"), is(testDate));
	}

	@Test
	public void testGetAsObjectValidFormatPM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "Mar-01-2020 14:30"), is(testDate));
	}

	@Test
	public void testGetAsStringAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("Mar-01-2020 02:30"));
	}

	@Test
	public void testGetAsStringPM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("Mar-01-2020 14:30"));
	}
}
