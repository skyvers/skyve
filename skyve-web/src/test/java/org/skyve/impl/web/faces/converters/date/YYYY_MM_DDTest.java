package org.skyve.impl.web.faces.converters.date;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
class YYYY_MM_DDTest {
	private YYYY_MM_DD converter = new YYYY_MM_DD();

	@Test
	void testGetAsObjectInvalidFormat() {
		// call the method under test
		assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, "2020-03-01"));
	}

	@Test
	void testGetAsObjectValidFormat() {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assertions.assertEquals(testDate, converter.getAsObject(null, null, "2020/03/01"));
	}

	@Test
	void testGetAsString() {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		Assertions.assertEquals("2020/03/01", converter.getAsString(null, null, testDate));
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