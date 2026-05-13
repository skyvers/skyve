package org.skyve.domain.types.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.model.Attribute.AttributeType;

/** Unit tests for plain decimal converters. */
@SuppressWarnings("static-method")
class DecimalConvertersTest {

	// ---- Decimal2Converter ----

	@Test
	void decimal2ConverterGetValueTypeIsDecimal2() {
		assertThat(new Decimal2Converter().getValueType().getSimpleName(), is("Decimal2"));
	}

	@Test
	void decimal2ConverterGetAttributeTypeIsDecimal2() {
		assertThat(new Decimal2Converter().getAttributeType(), is(AttributeType.decimal2));
	}

	@Test
	void decimal2ConverterGetFormatIsNull() {
		assertThat(new Decimal2Converter().getFormat(), is(nullValue()));
	}

	@Test
	void decimal2ConverterGetValidatorIsNull() {
		assertThat(new Decimal2Converter().getValidator(), is(nullValue()));
	}

	@Test
	void decimal2ConverterGetFormatPatternIsNull() {
		assertThat(new Decimal2Converter().getFormatPattern(), is(nullValue()));
	}

	@Test
	void decimal2ConverterToDisplayValueReturnsString() throws ConversionException {
		String result = new Decimal2Converter().toDisplayValue(new Decimal2("1.23"));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2ConverterFromDisplayValueReturnsDecimal2() throws ConversionException {
		Decimal2 result = new Decimal2Converter().fromDisplayValue("1.23");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2ConverterFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal2Converter().fromDisplayValue("notanumber"));
	}

	// ---- Decimal5Converter ----

	@Test
	void decimal5ConverterGetValueTypeIsDecimal5() {
		assertThat(new Decimal5Converter().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5ConverterGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5Converter().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5ConverterGetFormatIsNull() {
		assertThat(new Decimal5Converter().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5ConverterGetValidatorIsNull() {
		assertThat(new Decimal5Converter().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5ConverterGetFormatPatternIsNull() {
		assertThat(new Decimal5Converter().getFormatPattern(), is(nullValue()));
	}

	@Test
	void decimal5ConverterToDisplayValueReturnsString() throws ConversionException {
		String result = new Decimal5Converter().toDisplayValue(new Decimal5("1.23456"));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5ConverterFromDisplayValueReturnsDecimal5() throws ConversionException {
		Decimal5 result = new Decimal5Converter().fromDisplayValue("1.23456");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5ConverterFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5Converter().fromDisplayValue("notanumber"));
	}

	// ---- Decimal10Converter ----

	@Test
	void decimal10ConverterGetValueTypeIsDecimal10() {
		assertThat(new Decimal10Converter().getValueType().getSimpleName(), is("Decimal10"));
	}

	@Test
	void decimal10ConverterGetAttributeTypeIsDecimal10() {
		assertThat(new Decimal10Converter().getAttributeType(), is(AttributeType.decimal10));
	}

	@Test
	void decimal10ConverterGetFormatIsNull() {
		assertThat(new Decimal10Converter().getFormat(), is(nullValue()));
	}

	@Test
	void decimal10ConverterGetValidatorIsNull() {
		assertThat(new Decimal10Converter().getValidator(), is(nullValue()));
	}

	@Test
	void decimal10ConverterGetFormatPatternIsNull() {
		assertThat(new Decimal10Converter().getFormatPattern(), is(nullValue()));
	}

	@Test
	void decimal10ConverterToDisplayValueReturnsString() throws ConversionException {
		String result = new Decimal10Converter().toDisplayValue(new Decimal10("1.1234567890"));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10ConverterFromDisplayValueReturnsDecimal10() throws ConversionException {
		Decimal10 result = new Decimal10Converter().fromDisplayValue("1.1234567890");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10ConverterFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal10Converter().fromDisplayValue("notanumber"));
	}
}
