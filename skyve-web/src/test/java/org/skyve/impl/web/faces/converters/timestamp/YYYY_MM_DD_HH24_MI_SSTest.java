package org.skyve.impl.web.faces.converters.timestamp;

import org.junit.jupiter.api.Assertions;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
class YYYY_MM_DD_HH24_MI_SSTest {
	private YYYY_MM_DD_HH24_MI_SS converter = new YYYY_MM_DD_HH24_MI_SS();

	@Test
	void testGetAsObjectInvalidFormat() {
		// call the method under test
		assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, "2020-03-01 02:30:05"));
	}

	@Test
	void testGetAsObjectValidFormatAM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 05);

		// call the method under test
		Assertions.assertEquals(testDate, converter.getAsObject(null, null, "2020/03/01 02:30:05"));
	}

	@Test
	void testGetAsObjectValidFormatPM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 05);

		// call the method under test
		Assertions.assertEquals(testDate, converter.getAsObject(null, null, "2020/03/01 14:30:05"));
	}

	@Test
	void testGetAsStringAM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		Assertions.assertEquals("2020/03/01 02:30:55", converter.getAsString(null, null, testDate));
	}

	@Test
	void testGetAsStringPM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		Assertions.assertEquals("2020/03/01 14:30:55", converter.getAsString(null, null, testDate));
	}

	@Test
	void testGetAsObjectNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}
}