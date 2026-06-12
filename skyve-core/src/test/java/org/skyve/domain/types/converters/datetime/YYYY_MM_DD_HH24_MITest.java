package org.skyve.domain.types.converters.datetime;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

class YYYY_MM_DD_HH24_MITest {

	private YYYY_MM_DD_HH24_MI converter = new YYYY_MM_DD_HH24_MI();

	@Test
	void testFromDisplayValueInvalidFormat() {
		ConversionException ce = assertThrows(ConversionException.class, () -> converter.fromDisplayValue("01-03-2020 02:30"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueAM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assertions.assertEquals(testDate, converter.fromDisplayValue("2020/03/01 02:30"));
	}

	@Test
	void testFromDisplayValuePM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assertions.assertEquals(testDate, converter.fromDisplayValue("2020/03/01 14:30"));
	}

	@Test
	void testToDisplayValueAM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assertions.assertEquals("2020/03/01 02:30", converter.toDisplayValue(testDate));
	}

	@Test
	void testToDisplayValuePM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assertions.assertEquals("2020/03/01 14:30", converter.toDisplayValue(testDate));
	}
}
