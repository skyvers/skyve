package org.skyve.impl.web.faces.converters.time;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.TimeOnly;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
class HH_MITest {
	private HH_MI formatter;

	@BeforeEach
	void before() {
		formatter = new HH_MI();
	}

	@Test
	void testGetAsObjectInvalidFormat() {
		// call the method under test
		assertThrows(ConverterException.class, () -> formatter.getAsObject(null, null, "03-01-2020 02:30:05"));
	}

	@Test
	void testGetAsObjectValidFormatAM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 00);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "02:30 AM"), is(testDate));
	}

	@Test
	void testGetAsObjectValidFormatPM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 00);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "02:30 PM"), is(testDate));
	}

	@Test
	void testGetAsStringAM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("02:30 AM"));
	}

	@Test
	void testGetAsStringPM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("02:30 PM"));
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