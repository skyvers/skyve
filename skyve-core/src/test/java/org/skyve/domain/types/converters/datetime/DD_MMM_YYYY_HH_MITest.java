package org.skyve.domain.types.converters.datetime;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

class DD_MMM_YYYY_HH_MITest {

	private DD_MMM_YYYY_HH_MI formatter;

	@BeforeEach
	void before() {
		formatter = new DD_MMM_YYYY_HH_MI();
	}

	@Test
	void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> {
			// setup the test data
			DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
			Time.setTime(testDate, 02, 30, 0);

			// call the method under test
			assertThat(formatter.fromDisplayValue("01-03-2020 02:30"), is(testDate));
		});

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01-Mar-2020 02:30 AM"), is(testDate));
	}

	@Test
	void testFromDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01-Mar-2020 02:30 PM"), is(testDate));
	}

	@Test
	void testToDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01-Mar-2020 02:30 AM"));
	}

	@Test
	void testToDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01-Mar-2020 02:30 PM"));
	}
}
