package org.skyve.domain.types.converters.time;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.util.Time;

class HH24_MITest {

	private HH24_MI formatter;

	@BeforeEach
	void before() {
		formatter = new HH24_MI();
	}

	@Test
	void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> {
			// setup the test data
			TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
			Time.setTime(testDate, 02, 30, 05);

			// call the method under test
			assertThat(formatter.fromDisplayValue("02-30"), is(testDate));
		});

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 00);

		// call the method under test
		assertThat(formatter.fromDisplayValue("02:30"), is(testDate));
	}

	@Test
	void testFromDisplayValuePM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 00);

		// call the method under test
		assertThat(formatter.fromDisplayValue("14:30"), is(testDate));
	}

	@Test
	void testToDisplayValueAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("02:30"));
	}

	@Test
	void testToDisplayValuePM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("14:30"));
	}
}
