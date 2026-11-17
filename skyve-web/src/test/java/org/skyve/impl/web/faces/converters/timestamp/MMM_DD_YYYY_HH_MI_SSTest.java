package org.skyve.impl.web.faces.converters.timestamp;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
class MMM_DD_YYYY_HH_MI_SSTest {
	private MMM_DD_YYYY_HH_MI_SS formatter;

	@BeforeEach
	void before() {
		formatter = new MMM_DD_YYYY_HH_MI_SS();
	}

	@Test
	void testGetAsObjectInvalidFormat() {
		// call the method under test
		assertThrows(ConverterException.class, () -> formatter.getAsObject(null, null, "01-03-2020 02:30:55"));
	}

	@Test
	void testGetAsObjectValidFormatAM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "Mar-01-2020 02:30:55 AM"), is(testDate));
	}

	@Test
	void testGetAsObjectTrimsSurroundingSpaces() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test with the value padded by spaces
		assertThat(formatter.getAsObject(null, null, "  Mar-01-2020 02:30:55 AM  "), is(testDate));
	}

	@Test
	void testGetAsObjectValidFormatPM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "Mar-01-2020 02:30:55 PM"), is(testDate));
	}

	@Test
	void testGetAsStringAM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("Mar-01-2020 02:30:55 AM"));
	}

	@Test
	void testGetAsStringPM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("Mar-01-2020 02:30:55 PM"));
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