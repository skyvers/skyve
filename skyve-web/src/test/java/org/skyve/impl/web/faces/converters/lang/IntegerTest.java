package org.skyve.impl.web.faces.converters.lang;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import javax.faces.convert.ConverterException;

import org.junit.Before;
import org.junit.Test;

public class IntegerTest {

	private Integer converter;

	@Before
	public void before() {
		converter = new Integer();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidStringValue() throws Exception {
		// call the method under test
		converter.getAsObject(null, null, "not an integer");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectInvalidTooLargeValue() throws Exception {
		// call the method under test
		try {
			converter.getAsObject(null, null, "99999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("exceeds Integer max value"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectInvalidTooLargePositiveValue() throws Exception {
		// call the method under test
		try {
			converter.getAsObject(null, null, "+99999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("exceeds Integer max value"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectInvalidTooSmallValue() throws Exception {
		// call the method under test
		try {
			converter.getAsObject(null, null, "-99999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("exceeds Integer min value"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidValue() throws Exception {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsObject(null, null, "1000"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidLargeValue() throws Exception {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf("999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "999999999"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidSmallValue() throws Exception {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf("-999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "-999999999"), is(testValue));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		java.lang.Integer testValue = java.lang.Integer.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1000"));
	}
}
