package org.skyve.domain.types.converters.timestamp;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.text.ParseException;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

public class MMM_DD_YYYY_HH24_MI_SSTest {

	private MMM_DD_YYYY_HH24_MI_SS formatter;

	@Before
	public void before() {
		formatter = new MMM_DD_YYYY_HH24_MI_SS();
	}

	@Test(expected = ParseException.class)
	public void testFromDisplayValueInvalidFormat() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 05);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01-03-2020 02:30:05"), is(testDate));
	}

	@Test
	public void testFromDisplayValueAM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.fromDisplayValue("Mar-01-2020 02:30:55"), is(testDate));
	}

	@Test
	public void testFromDisplayValuePM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.fromDisplayValue("Mar-01-2020 14:30:55"), is(testDate));
	}

	@Test
	public void testToDisplayValueAM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("Mar-01-2020 02:30:55"));
	}

	@Test
	public void testToDisplayValuePM() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("Mar-01-2020 14:30:55"));
	}
}
