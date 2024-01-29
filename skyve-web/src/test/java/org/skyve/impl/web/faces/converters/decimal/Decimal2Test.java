package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;

public class Decimal2Test {
	private Decimal2Converter converter;

	@Before
	public void before() {
		converter = new Decimal2Converter();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidStringValue() throws Exception {
		// call the method under test
		converter.getAsObject(null, null, "not a Decimal2");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectLargeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectLargePositiveValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "+9999999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectLargeNegativeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
	}

	@Test
	public void testGetAsObjectValidValue() throws Exception {
		// setup the test data
		org.skyve.domain.types.Decimal2 testValue = org.skyve.domain.types.Decimal2.ONE_THOUSAND;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "1000"), is(testValue));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		org.skyve.domain.types.Decimal2 testValue = org.skyve.domain.types.Decimal2.ONE_THOUSAND;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1000.00"));
	}

	@Test
	public void testGetAsStringPrecisionValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "99.99";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue));
	}

	@Test
	public void testGetAsStringLargePrecisionValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "+99.99999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is("100.00"));
	}

	@Test
	public void testGetAsStringLargePositiveValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2("+" + stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".00"));
	}

	@Test
	public void testGetAsStringLargeNegativeValue() throws Exception {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".00"));
	}
}
