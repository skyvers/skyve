package org.skyve.domain.types.converters.datetime;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

public class YYYY_MM_DD_HH_MITest {

	private YYYY_MM_DD_HH_MI converter = new YYYY_MM_DD_HH_MI();

	@Test
	public void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> {
			// call the method under test
			converter.fromDisplayValue("01-03-2020 02:30");
		});

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	public void testFromDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assert.assertEquals(testDate, converter.fromDisplayValue("2020/03/01 02:30 AM"));
	}

	@Test
	public void testFromDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assert.assertEquals(testDate, converter.fromDisplayValue("2020/03/01 02:30 PM"));
	}

	@Test
	public void testToDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assert.assertEquals("2020/03/01 02:30 AM", converter.toDisplayValue(testDate));
	}

	@Test
	public void testToDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assert.assertEquals("2020/03/01 02:30 PM", converter.toDisplayValue(testDate));
	}
}
