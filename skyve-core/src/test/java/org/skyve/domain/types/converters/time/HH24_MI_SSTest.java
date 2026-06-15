package org.skyve.domain.types.converters.time;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.util.Time;

class HH24_MI_SSTest {

	private HH24_MI_SS formatter;

	@BeforeEach
	void before() {
		formatter = new HH24_MI_SS();
	}

	@Test
	void testFromDisplayValueInvalidFormat() {
		ConversionException ce = assertThrows(ConversionException.class, () -> formatter.fromDisplayValue("02:30"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueAM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.fromDisplayValue("02:30:55"), is(testDate));
	}

	@Test
	void testFromDisplayValuePM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.fromDisplayValue("14:30:55"), is(testDate));
	}

	@Test
	void testToDisplayValueAM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("02:30:55"));
	}

	@Test
	void testToDisplayValuePM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("14:30:55"));
	}

	@Test
	void testGetFormatPattern() {
		assertThat(formatter.getFormatPattern(), is(HH24_MI_SS.PATTERN));
	}

	@Test
	void testGetFormat() {
		assertThat(formatter.getFormat(), org.hamcrest.Matchers.notNullValue());
	}
}
