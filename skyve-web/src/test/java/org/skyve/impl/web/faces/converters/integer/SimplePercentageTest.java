package org.skyve.impl.web.faces.converters.integer;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.faces.convert.ConverterException;
class SimplePercentageTest {
	private SimplePercentage converter;

	@BeforeEach
	void before() {
		converter = new SimplePercentage();
	}

	@Test
	void testGetAsObjectInvalidStringValue() {
		// call the method under test
		assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, "not an integer"));
	}

	@Test
	void testGetAsObjectInvalidTooLargeValue() {
		// call the method under test
		try {
			converter.getAsObject(null, null, "99999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("must not be greater than"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	void testGetAsObjectInvalidTooLargePositiveValue() {
		// call the method under test
		try {
			converter.getAsObject(null, null, "+99999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("must not be greater than"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	void testGetAsObjectInvalidTooSmallValue() {
		// call the method under test
		try {
			converter.getAsObject(null, null, "-99999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("must not be less than"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	void testGetAsObjectValidValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsObject(null, null, "1000"), is(testValue));
	}

	@Test
	void testGetAsObjectValidLargeValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf("999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "999999999"), is(testValue));
	}

	@Test
	void testGetAsObjectValidSmallValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf("-999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "-999999999"), is(testValue));
	}

	@Test
	void testGetAsString() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1000%"));
	}

	@Test
	void testGetAsStringLargeValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000000);

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1000000%"));
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