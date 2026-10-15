package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.util.Util;

import jakarta.faces.convert.ConverterException;

class Decimal2Test {
	private static final String EXPECTED_MESSAGE = Util.nullSafeI18n(ConversionException.DECIMAL_2_CONVERTER_KEY);

	private Decimal2Converter converter;

	@BeforeEach
	void before() {
		converter = new Decimal2Converter();
	}

	@Test
	void testGetAsObjectInvalidStringValue() {
		ConverterException e = assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, "not a Decimal2"));
		assertThat(e.getFacesMessage().getSummary(), is(EXPECTED_MESSAGE));
	}

	@Test
	void testGetAsObjectLargeValues() {
		for (String stringValue : new String[] {"9999999999999999999", "+9999999999999999999", "-9999999999999999999"}) {
			org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);
			assertThat(converter.getAsObject(null, null, stringValue), is(decimalValue));
		}
	}

	@Test
	void testGetAsObjectValidValue() {
		// setup the test data
		org.skyve.domain.types.Decimal2 testValue = org.skyve.domain.types.Decimal2.ONE_THOUSAND;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "1000"), is(testValue));
	}

	@Test
	void testGetAsString() {
		// setup the test data
		org.skyve.domain.types.Decimal2 testValue = org.skyve.domain.types.Decimal2.ONE_THOUSAND;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("1000.00"));
	}

	@Test
	void testGetAsStringPrecisionValue() {
		// setup the test data
		java.lang.String stringValue = "99.99";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue));
	}

	@Test
	void testGetAsStringLargePrecisionValue() {
		// setup the test data
		java.lang.String stringValue = "+99.99999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is("100.00"));
	}

	@Test
	void testGetAsStringLargePositiveValue() {
		// setup the test data
		java.lang.String stringValue = "9999999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2("+" + stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".00"));
	}

	@Test
	void testGetAsStringLargeNegativeValue() {
		// setup the test data
		java.lang.String stringValue = "-9999999999999999999";
		org.skyve.domain.types.Decimal2 decimalValue = new org.skyve.domain.types.Decimal2(stringValue);

		// call the method under test
		assertThat(converter.getAsString(null, null, decimalValue), is(stringValue + ".00"));
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
