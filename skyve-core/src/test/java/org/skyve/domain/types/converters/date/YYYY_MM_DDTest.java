package org.skyve.domain.types.converters.date;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.Time;

public class YYYY_MM_DDTest {

	private YYYY_MM_DD converter = new YYYY_MM_DD();

	@Test
	public void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> {
			// call the method under test
			converter.fromDisplayValue("03-01-2020");
		});

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	public void testFromDisplayValueValidFormat() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assert.assertEquals(testDate, converter.fromDisplayValue("2020/03/01"));
	}

	@Test
	public void testToDisplayValue() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assert.assertEquals("2020/03/01", converter.toDisplayValue(testDate));
	}
}
