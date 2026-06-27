package org.skyve.domain.types.converters.timestamp;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

class YYYY_MM_DDTest {

	private YYYY_MM_DD converter = new YYYY_MM_DD();

	@Test
	void testFromDisplayValueInvalidFormat() {
		ConversionException ce = assertThrows(ConversionException.class, () -> converter.fromDisplayValue("2020-03-01"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValue() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		Assertions.assertEquals(testDate, converter.fromDisplayValue("2020/03/01"));
	}

	@Test
	void testToDisplayValue() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		Assertions.assertEquals("2020/03/01", converter.toDisplayValue(testDate));
	}
}
