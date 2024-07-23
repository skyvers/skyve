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

public class HH_MITest {

	private HH_MI formatter;

	@BeforeEach
	public void before() {
		formatter = new HH_MI();
	}

	@Test
	public void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> {
			// setup the test data
			TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
			Time.setTime(testDate, 02, 30, 05);

			// call the method under test
			assertThat(formatter.fromDisplayValue("02:30:05"), is(testDate));
		});

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	public void testFromDisplayValueAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 00);

		// call the method under test
		assertThat(formatter.fromDisplayValue("02:30 AM"), is(testDate));
	}

	@Test
	public void testFromDisplayValuePM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 00);

		// call the method under test
		assertThat(formatter.fromDisplayValue("02:30 PM"), is(testDate));
	}

	@Test
	public void testToDisplayValueAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("02:30 AM"));
	}

	@Test
	public void testToDisplayValuePM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("02:30 PM"));
	}
}
