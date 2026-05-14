package org.skyve.domain.types.converters.integer;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.metadata.model.Attribute.AttributeType;

/** Unit tests for integer converter classes. */
@SuppressWarnings("static-method")
class IntegerConvertersTest {

	// ---- IntegerConverter ----

	@Test
	void integerConverterGetValueTypeIsInteger() {
		assertThat(new IntegerConverter().getValueType().getSimpleName(), is("Integer"));
	}

	@Test
	void integerConverterGetAttributeTypeIsInteger() {
		assertThat(new IntegerConverter().getAttributeType(), is(AttributeType.integer));
	}

	@Test
	void integerConverterGetFormatIsNull() {
		assertThat(new IntegerConverter().getFormat(), is(nullValue()));
	}

	@Test
	void integerConverterGetValidatorIsNull() {
		assertThat(new IntegerConverter().getValidator(), is(nullValue()));
	}

	@Test
	void integerConverterGetFormatPatternIsNull() {
		assertThat(new IntegerConverter().getFormatPattern(), is(nullValue()));
	}

	@Test
	void integerConverterToDisplayValueReturnsString() throws ConversionException {
		assertThat(new IntegerConverter().toDisplayValue(Integer.valueOf(42)), is("42"));
	}

	@Test
	void integerConverterFromDisplayValueReturnsInteger() throws ConversionException {
		assertThat(new IntegerConverter().fromDisplayValue("42"), is(Integer.valueOf(42)));
	}

	@Test
	void integerConverterFromDisplayValueNegativeReturnsInteger() throws ConversionException {
		assertThat(new IntegerConverter().fromDisplayValue("-7"), is(Integer.valueOf(-7)));
	}

	@Test
	void integerConverterFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new IntegerConverter().fromDisplayValue("notanumber"));
	}

	@Test
	void integerConverterFromDisplayValueLargePositiveThrows() {
		assertThrows(ConversionException.class, () -> new IntegerConverter().fromDisplayValue("12345678901"));
	}

	@Test
	void integerConverterFromDisplayValueLargeNegativeThrows() {
		assertThrows(ConversionException.class, () -> new IntegerConverter().fromDisplayValue("-12345678901"));
	}

	// ---- IntegerSeparator ----

	@Test
	void integerSeparatorGetValueTypeIsInteger() {
		assertThat(new IntegerSeparator().getValueType().getSimpleName(), is("Integer"));
	}

	@Test
	void integerSeparatorGetAttributeTypeIsInteger() {
		assertThat(new IntegerSeparator().getAttributeType(), is(AttributeType.integer));
	}

	@Test
	void integerSeparatorGetFormatIsNull() {
		assertThat(new IntegerSeparator().getFormat(), is(nullValue()));
	}

	@Test
	void integerSeparatorGetValidatorIsNull() {
		assertThat(new IntegerSeparator().getValidator(), is(nullValue()));
	}

	@Test
	void integerSeparatorGetFormatPatternIsNull() {
		assertThat(new IntegerSeparator().getFormatPattern(), is(nullValue()));
	}

	@Test
	void integerSeparatorToDisplayValueFormatsWithCommas() throws ConversionException {
		String result = new IntegerSeparator().toDisplayValue(Integer.valueOf(1000000));
		assertThat(result, is("1,000,000"));
	}

	@Test
	void integerSeparatorFromDisplayValueStripsCommas() throws ConversionException {
		assertThat(new IntegerSeparator().fromDisplayValue("1,000,000"), is(Integer.valueOf(1000000)));
	}

	@Test
	void integerSeparatorFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new IntegerSeparator().fromDisplayValue("notanumber"));
	}

	@Test
	void integerSeparatorFromDisplayValueLargePositiveThrows() {
		assertThrows(ConversionException.class, () -> new IntegerSeparator().fromDisplayValue("12345678901"));
	}

	@Test
	void integerSeparatorFromDisplayValueLargeNegativeThrows() {
		assertThrows(ConversionException.class, () -> new IntegerSeparator().fromDisplayValue("-12345678901"));
	}

	// ---- LongIntegerConverter ----

	@Test
	void longIntegerConverterGetValueTypeIsLong() {
		assertThat(new LongIntegerConverter().getValueType().getSimpleName(), is("Long"));
	}

	@Test
	void longIntegerConverterGetAttributeTypeIsLongInteger() {
		assertThat(new LongIntegerConverter().getAttributeType(), is(AttributeType.longInteger));
	}

	@Test
	void longIntegerConverterGetFormatIsNull() {
		assertThat(new LongIntegerConverter().getFormat(), is(nullValue()));
	}

	@Test
	void longIntegerConverterGetValidatorIsNull() {
		assertThat(new LongIntegerConverter().getValidator(), is(nullValue()));
	}

	@Test
	void longIntegerConverterGetFormatPatternIsNull() {
		assertThat(new LongIntegerConverter().getFormatPattern(), is(nullValue()));
	}

	@Test
	void longIntegerConverterToDisplayValueReturnsString() throws ConversionException {
		assertThat(new LongIntegerConverter().toDisplayValue(Long.valueOf(9876543210L)), is("9876543210"));
	}

	@Test
	void longIntegerConverterFromDisplayValueReturnsLong() throws ConversionException {
		assertThat(new LongIntegerConverter().fromDisplayValue("9876543210"), is(Long.valueOf(9876543210L)));
	}

	@Test
	void longIntegerConverterFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new LongIntegerConverter().fromDisplayValue("notanumber"));
	}

	@Test
	void longIntegerConverterFromDisplayValueLargePositiveThrows() {
		assertThrows(ConversionException.class, () -> new LongIntegerConverter().fromDisplayValue("99999999999999999999"));
	}

	@Test
	void longIntegerConverterFromDisplayValueLargeNegativeThrows() {
		assertThrows(ConversionException.class, () -> new LongIntegerConverter().fromDisplayValue("-99999999999999999999"));
	}

	// ---- LongIntegerSeparator ----

	@Test
	void longIntegerSeparatorGetValueTypeIsLong() {
		assertThat(new LongIntegerSeparator().getValueType().getSimpleName(), is("Long"));
	}

	@Test
	void longIntegerSeparatorGetAttributeTypeIsLongInteger() {
		assertThat(new LongIntegerSeparator().getAttributeType(), is(AttributeType.longInteger));
	}

	@Test
	void longIntegerSeparatorGetFormatIsNull() {
		assertThat(new LongIntegerSeparator().getFormat(), is(nullValue()));
	}

	@Test
	void longIntegerSeparatorGetValidatorIsNull() {
		assertThat(new LongIntegerSeparator().getValidator(), is(nullValue()));
	}

	@Test
	void longIntegerSeparatorGetFormatPatternIsNull() {
		assertThat(new LongIntegerSeparator().getFormatPattern(), is(nullValue()));
	}

	@Test
	void longIntegerSeparatorToDisplayValueFormatsWithCommas() throws ConversionException {
		String result = new LongIntegerSeparator().toDisplayValue(Long.valueOf(1000000L));
		assertThat(result, is("1,000,000"));
	}

	@Test
	void longIntegerSeparatorFromDisplayValueStripsCommas() throws ConversionException {
		assertThat(new LongIntegerSeparator().fromDisplayValue("1,000,000"), is(Long.valueOf(1000000L)));
	}

	@Test
	void longIntegerSeparatorFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new LongIntegerSeparator().fromDisplayValue("notanumber"));
	}

	@Test
	void longIntegerSeparatorFromDisplayValueLargePositiveThrows() {
		assertThrows(ConversionException.class, () -> new LongIntegerSeparator().fromDisplayValue("99999999999999999999"));
	}

	@Test
	void longIntegerSeparatorFromDisplayValueLargeNegativeThrows() {
		assertThrows(ConversionException.class, () -> new LongIntegerSeparator().fromDisplayValue("-99999999999999999999"));
	}

	// ---- SimplePercentage ----

	@Test
	void simplePercentageGetValueTypeIsInteger() {
		assertThat(new SimplePercentage().getValueType().getSimpleName(), is("Integer"));
	}

	@Test
	void simplePercentageGetAttributeTypeIsInteger() {
		assertThat(new SimplePercentage().getAttributeType(), is(AttributeType.integer));
	}

	@Test
	void simplePercentageGetFormatIsNull() {
		assertThat(new SimplePercentage().getFormat(), is(nullValue()));
	}

	@Test
	void simplePercentageGetValidatorIsNull() {
		assertThat(new SimplePercentage().getValidator(), is(nullValue()));
	}

	@Test
	void simplePercentageToDisplayValueAddsPercentSign() throws ConversionException {
		assertThat(new SimplePercentage().toDisplayValue(Integer.valueOf(75)), is("75%"));
	}

	@Test
	void simplePercentageFromDisplayValueStripsPercentSign() throws ConversionException {
		assertThat(new SimplePercentage().fromDisplayValue("75%"), is(Integer.valueOf(75)));
	}

	@Test
	void simplePercentageFromDisplayValueNoPercentSign() throws ConversionException {
		assertThat(new SimplePercentage().fromDisplayValue("100"), is(Integer.valueOf(100)));
	}

	@Test
	void simplePercentageFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new SimplePercentage().fromDisplayValue("notanumber%"));
	}

	@Test
	void simplePercentageGetFormatPatternIsNull() {
		assertThat(new SimplePercentage().getFormatPattern(), is(nullValue()));
	}

	@Test
	void simplePercentageFromDisplayValueLargePositiveThrows() {
		assertThrows(ConversionException.class, () -> new SimplePercentage().fromDisplayValue("12345678901%"));
	}

	@Test
	void simplePercentageFromDisplayValueLargeNegativeThrows() {
		assertThrows(ConversionException.class, () -> new SimplePercentage().fromDisplayValue("-12345678901%"));
	}
}
