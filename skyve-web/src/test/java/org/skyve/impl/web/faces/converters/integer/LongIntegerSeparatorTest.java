package org.skyve.impl.web.faces.converters.integer;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import javax.faces.convert.ConverterException;

import org.junit.Before;
import org.junit.Test;

public class LongIntegerSeparatorTest {
	private LongIntegerSeparator converter;

	@Before
	public void before() {
		converter = new LongIntegerSeparator();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidStringValue() throws Exception {
		// call the method under test
		converter.getAsObject(null, null, "not an Long");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectInvalidTooLargeValue() throws Exception {
		// call the method under test
		try {
			converter.getAsObject(null, null, "9999999999999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("must not be greater than"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectInvalidTooLargePositiveValue() throws Exception {
		// call the method under test
		try {
			converter.getAsObject(null, null, "+9999999999999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("must not be greater than"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectInvalidTooSmallValue() throws Exception {
		// call the method under test
		try {
			converter.getAsObject(null, null, "-9999999999999999999");
		} catch (ConverterException e) {
			assertThat(e.getMessage(), containsString("must not be less than"));
			return;
		}

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidValue() throws Exception {
		// setup the test data
		java.lang.Long testValue = java.lang.Long.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsObject(null, null, "1000"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidLargeValue() throws Exception {
		// setup the test data
		java.lang.Long testValue = java.lang.Long.valueOf("999999999999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "999999999999999999"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidSmallValue() throws Exception {
		// setup the test data
		java.lang.Long testValue = java.lang.Long.valueOf("-999999999999999999");

		// call the method under test
		assertThat(converter.getAsObject(null, null, "-999999999999999999"), is(testValue));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		java.lang.Long testValue = java.lang.Long.valueOf(1000);

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1,000"));
	}

	@Test
	public void testGetAsStringLargeValue() throws Exception {
		// setup the test data
		java.lang.Long testValue = java.lang.Long.valueOf(1000000000);

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1,000,000,000"));
	}
}
