package org.skyve.impl.web.faces.converters.lang;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

import javax.faces.convert.ConverterException;

import org.junit.Before;
import org.junit.Test;

public class Decimal10Test {

	private Decimal10 converter;

	@Before
	public void before() {
		converter = new Decimal10();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidStringValue() throws Exception {
		// call the method under test
		converter.getAsObject(null, null, "not a Decimal10");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectLargeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectLargePositiveValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "+9999999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectLargeNegativeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectValidValue() throws Exception {
		// setup the test data
		org.skyve.domain.types.Decimal10 testValue = org.skyve.domain.types.Decimal10.ONE_HUNDRED;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "100"), is(testValue));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		org.skyve.domain.types.Decimal10 testValue = org.skyve.domain.types.Decimal10.ONE_HUNDRED;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("100.0000000000"));
	}

	@Test
	public void testGetAsStringPrecisionValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "99.9999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue));
	}

	@Test
	public void testGetAsStringLargePrecisionValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "+99.99999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is("100.0000000000"));
	}

	@Test
	public void testGetAsStringLargePositiveValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10("+" + stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".0000000000"));
	}

	@Test
	public void testGetAsStringLargeNegativeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".0000000000"));
	}
}
