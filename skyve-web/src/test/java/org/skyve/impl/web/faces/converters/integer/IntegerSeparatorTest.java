package org.skyve.impl.web.faces.converters.integer;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;
public class IntegerSeparatorTest {
	private IntegerSeparator converter;

	@Before
	public void before() {
		converter = new IntegerSeparator();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidStringValue() {
		// call the method under test
		converter.getAsObject(null, null, "not an integer");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectInvalidTooLargeValue() {
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
	public void testGetAsObjectInvalidTooLargePositiveValue() {
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
	public void testGetAsObjectInvalidTooSmallValue() {
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
	public void testGetAsObjectValidValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsObject(null, null, "1000"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidLargeValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf("999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "999999999"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidSmallValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf("-999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "-999999999"), is(testValue));
	}

	@Test
	public void testGetAsString() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1,000"));
	}

	@Test
	public void testGetAsStringLargeValue() {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000000);

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1,000,000"));
	}

	@Test
	public void testGetAsObjectNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	public void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}
}