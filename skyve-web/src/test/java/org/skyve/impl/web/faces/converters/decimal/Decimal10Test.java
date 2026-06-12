package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;
public class Decimal10Test {
	private Decimal10Converter converter;

	@Before
	public void before() {
		converter = new Decimal10Converter();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidStringValue() {
		// call the method under test
		converter.getAsObject(null, null, "not a Decimal10");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectLargeValues() {
		for (String stringValue : new String[] {"9999999999999999999", "+9999999999999999999", "-9999999999999999999"}) {
			org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);
			assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
		}
	}

	@Test
	public void testGetAsObjectValidValue() {
		// setup the test data
		org.skyve.domain.types.Decimal10 testValue = org.skyve.domain.types.Decimal10.ONE_HUNDRED;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "100"), is(testValue));
	}

	@Test
	public void testGetAsString() {
		// setup the test data
		org.skyve.domain.types.Decimal10 testValue = org.skyve.domain.types.Decimal10.ONE_HUNDRED;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("100.0000000000"));
	}

	@Test
	public void testGetAsStringPrecisionValue() {
		// setup the test data
		java.lang.String stringValue = "99.9999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue));
	}

	@Test
	public void testGetAsStringLargePrecisionValue() {
		// setup the test data
		java.lang.String stringValue = "+99.99999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is("100.0000000000"));
	}

	@Test
	public void testGetAsStringLargePositiveValue() {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10("+" + stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".0000000000"));
	}

	@Test
	public void testGetAsStringLargeNegativeValue() {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal10 decimalValue = new org.skyve.domain.types.Decimal10(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".0000000000"));
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
