package org.skyve.impl.web.faces.converters.lang;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

import javax.faces.convert.ConverterException;

import org.junit.Before;
import org.junit.Test;

public class Decimal5Test {

	private Decimal5 converter;

	@Before
	public void before() {
		converter = new Decimal5();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidStringValue() throws Exception {
		// call the method under test
		converter.getAsObject(null, null, "not a Decimal5");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectLargeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal5 decimalValue = new org.skyve.domain.types.Decimal5(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectLargePositiveValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "+9999999999999999999";
		org.skyve.domain.types.Decimal5 decimalValue = new org.skyve.domain.types.Decimal5(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectLargeNegativeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal5 decimalValue = new org.skyve.domain.types.Decimal5(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectValidValue() throws Exception {
		// setup the test data
		org.skyve.domain.types.Decimal5 testValue = org.skyve.domain.types.Decimal5.ONE_HUNDRED;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "100"), is(testValue));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		org.skyve.domain.types.Decimal5 testValue = org.skyve.domain.types.Decimal5.ONE_HUNDRED;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("100.00000"));
	}

	@Test
	public void testGetAsStringPrecisionValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "99.99999";
		org.skyve.domain.types.Decimal5 decimalValue = new org.skyve.domain.types.Decimal5(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue));
	}

	@Test
	public void testGetAsStringLargePrecisionValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "+99.99999999999999999";
		org.skyve.domain.types.Decimal5 decimalValue = new org.skyve.domain.types.Decimal5(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is("100.00000"));
	}

	@Test
	public void testGetAsStringLargePositiveValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal5 decimalValue = new org.skyve.domain.types.Decimal5("+" + stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".00000"));
	}

	@Test
	public void testGetAsStringLargeNegativeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal5 decimalValue = new org.skyve.domain.types.Decimal5(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".00000"));
	}
}
