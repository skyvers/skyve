package org.skyve.domain.types.converters.date;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.Time;

class YYYY_MM_DDTest {

	private YYYY_MM_DD converter = new YYYY_MM_DD();

	@Test
	void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> converter.fromDisplayValue("03-01-2020"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueValidFormat() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assertions.assertEquals(testDate, converter.fromDisplayValue("2020/03/01"));
	}

	@Test
	void testToDisplayValue() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assertions.assertEquals("2020/03/01", converter.toDisplayValue(testDate));
	}
}
