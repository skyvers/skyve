package org.skyve.impl.web.faces.converters.datetime;

import org.junit.jupiter.api.Assertions;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
class YYYY_MM_DD_HH24_MITest {
	private YYYY_MM_DD_HH24_MI formatter = new YYYY_MM_DD_HH24_MI();

	@Test
	void testGetAsObjectInvalidFormat() {
		// call the method under test
		assertThrows(ConverterException.class, () -> formatter.getAsObject(null, null, "2020-01-03 02:30"));
	}

	@Test
	void testGetAsObjectValidFormatAM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assertions.assertEquals(testDate, formatter.getAsObject(null, null, "2020/03/01 02:30"));
	}

	@Test
	void testGetAsObjectValidFormatPM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assertions.assertEquals(testDate, formatter.getAsObject(null, null, "2020/03/01 14:30"));
	}

	@Test
	void testGetAsStringAM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		Assertions.assertEquals("2020/03/01 02:30", formatter.getAsString(null, null, testDate));
	}

	@Test
	void testGetAsStringPM() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		Assertions.assertEquals("2020/03/01 14:30", formatter.getAsString(null, null, testDate));
	}

	@Test
	void testGetAsObjectNullValue() {
		assertNull(formatter.getAsObject(null, null, null));
	}

	@Test
	void testGetAsStringNullValue() {
		assertThat(formatter.getAsString(null, null, null), is(""));
	}
}