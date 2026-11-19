package org.skyve.impl.web.faces.converters.decimal.currency;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.text.DecimalFormatSymbols;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.util.Util;

import jakarta.faces.convert.ConverterException;

class Decimal5DollarsAndCentsTest {
	private static final String EXPECTED_MESSAGE = Util.nullSafeI18n(ConversionException.DECIMAL_5_DOLLARS_AND_CENTS_KEY);

	private Decimal5DollarsAndCents converter;

	@BeforeEach
	void before() {
		converter = new Decimal5DollarsAndCents();
	}

	@Test
	void testGetAsObjectNullValue() {
		assertThat(converter.getAsObject(null, null, null), nullValue());
	}

	@Test
	void testGetAsObjectEmptyValue() {
		assertThat(converter.getAsObject(null, null, ""), nullValue());
	}

	@Test
	void testGetAsObjectInvalidValue() {
		ConverterException e = assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, "not a decimal"));
		assertThat(e.getFacesMessage().getSummary(), is(EXPECTED_MESSAGE));
	}

	@Test
	void testGetAsObjectValidValue() {
		org.skyve.domain.types.Decimal5 expected = new org.skyve.domain.types.Decimal5("3.14");
		assertThat(converter.getAsObject(null, null, "3.14"), is(expected));
	}

	@Test
	void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	void testGetAsStringValidValue() {
		org.skyve.domain.types.Decimal5 value = org.skyve.domain.types.Decimal5.ONE_HUNDRED;
		assertThat(converter.getAsString(null, null, value), is("100.00"));
	}

	@Test
	void testGetAsObjectNotANumber() {
		String nan = DecimalFormatSymbols.getInstance().getNaN();
		ConverterException e = assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, nan));
		assertThat(e.getFacesMessage().getSummary(), is(EXPECTED_MESSAGE));
	}
}
