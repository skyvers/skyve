package org.skyve.impl.web.faces.converters.decimal;

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

class Decimal5TwoDecimalPlacesPercentageTest {
	private static final String EXPECTED_MESSAGE = Util.nullSafeI18n(ConversionException.DECIMAL_5_TWO_DECIMAL_PLACES_PERCENTAGE_KEY);

	private Decimal5TwoDecimalPlacesPercentage converter;

	@BeforeEach
	void before() {
		converter = new Decimal5TwoDecimalPlacesPercentage();
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
		assertThat(converter.getAsObject(null, null, "50%"), is(new org.skyve.domain.types.Decimal5("0.5")));
	}

	@Test
	void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	void testGetAsStringValidValue() {
		// 0.5 Decimal5 → 50.00%
		org.skyve.domain.types.Decimal5 value = new org.skyve.domain.types.Decimal5("0.5");
		assertThat(converter.getAsString(null, null, value), is("50.00%"));
	}

	@Test
	void testGetAsObjectNotANumber() {
		String nan = DecimalFormatSymbols.getInstance().getNaN();
		ConverterException e = assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, nan));
		assertThat(e.getFacesMessage().getSummary(), is(EXPECTED_MESSAGE));
	}

	@Test
	void testGetAsObjectRoundsHalfUpToTheDisplayedPrecision() {
		assertThat(converter.getAsObject(null, null, "12.345%"), is(new org.skyve.domain.types.Decimal5("0.1235")));
	}
}
