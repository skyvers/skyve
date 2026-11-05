package org.skyve.impl.web.faces.converters.datetime;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
class MMM_DD_YYYYTest {
	private MMM_DD_YYYY formatter;

	@BeforeEach
	void before() {
		formatter = new MMM_DD_YYYY();
	}

	@Test
	void testGetAsObjectInvalidFormat() {
		// call the method under test
		assertThrows(ConverterException.class, () -> formatter.getAsObject(null, null, "01-03-2020"));
	}

	@Test
	void testGetAsObjectValidFormat() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "Mar-01-2020"), is(testDate));
	}

	@Test
	void testGetAsString() {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("Mar-01-2020"));
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