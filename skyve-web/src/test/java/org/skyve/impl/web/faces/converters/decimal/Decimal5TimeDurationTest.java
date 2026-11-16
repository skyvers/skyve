package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.util.Util;

import jakarta.faces.convert.ConverterException;

class Decimal5TimeDurationTest {
	private static final String EXPECTED_MESSAGE = Util.nullSafeI18n(ConversionException.DECIMAL_5_TIME_DURATION_KEY);

	private Decimal5TimeDuration converter;

	@BeforeEach
	void before() {
		converter = new Decimal5TimeDuration();
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
		ConverterException e = assertThrows(ConverterException.class, () -> converter.getAsObject(null, null, "not a duration"));
		assertThat(e.getFacesMessage().getSummary(), is(EXPECTED_MESSAGE));
	}

	@Test
	void testGetAsObjectValidValue() {
		// "1:30" represents 1 hour 30 minutes = 1.5
		org.skyve.domain.types.Decimal5 expected = new org.skyve.domain.types.Decimal5("1.5");
		assertThat(converter.getAsObject(null, null, "1:30"), is(expected));
	}

	@Test
	void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	void testGetAsStringValidValue() {
		org.skyve.domain.types.Decimal5 value = new org.skyve.domain.types.Decimal5("1.5");
		assertThat(converter.getAsString(null, null, value), is("1:30"));
	}
}
