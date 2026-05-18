package org.skyve.domain.types.converters.datetime;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

class DD_MM_YYYY_HH24_MITest {

	private DD_MM_YYYY_HH24_MI formatter;

	@BeforeEach
	void before() {
		formatter = new DD_MM_YYYY_HH24_MI();
	}

	@Test
	void testFromDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 02:30"), is(testDate));
	}

	@Test
	void testFromDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 14:30"), is(testDate));
	}

	@Test
	void testToDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 02:30"));
	}

	@Test
	void testToDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 14:30"));
	}
}
