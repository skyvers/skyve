package org.skyve.domain.types.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
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

	// ---- Decimal10TwoDecimalPlaces ----

	@Test
	void decimal10TwoDecimalPlacesGetValueTypeIsDecimal10() {
		assertThat(new Decimal10TwoDecimalPlaces().getValueType().getSimpleName(), is("Decimal10"));
	}

	@Test
	void decimal10TwoDecimalPlacesGetAttributeTypeIsDecimal10() {
		assertThat(new Decimal10TwoDecimalPlaces().getAttributeType(), is(AttributeType.decimal10));
	}

	@Test
	void decimal10TwoDecimalPlacesGetFormatIsNull() {
		assertThat(new Decimal10TwoDecimalPlaces().getFormat(), is(nullValue()));
	}

	@Test
	void decimal10TwoDecimalPlacesGetValidatorIsNull() {
		assertThat(new Decimal10TwoDecimalPlaces().getValidator(), is(nullValue()));
	}

	@Test
	void decimal10TwoDecimalPlacesToDisplayValueReturnsString() throws ConversionException {
		String result = new Decimal10TwoDecimalPlaces().toDisplayValue(new Decimal10("1234.56"));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10TwoDecimalPlacesFromDisplayValueReturnsDecimal10() throws ConversionException {
		Decimal10 result = new Decimal10TwoDecimalPlaces().fromDisplayValue("1,234.56");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10TwoDecimalPlacesFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal10TwoDecimalPlaces().fromDisplayValue("notanumber"));
	}

	// ---- Decimal2Integer ----

	@Test
	void decimal2IntegerGetValueTypeIsDecimal2() {
		assertThat(new Decimal2Integer().getValueType().getSimpleName(), is("Decimal2"));
	}

	@Test
	void decimal2IntegerGetAttributeTypeIsDecimal2() {
		assertThat(new Decimal2Integer().getAttributeType(), is(AttributeType.decimal2));
	}

	@Test
	void decimal2IntegerGetFormatIsNull() {
		assertThat(new Decimal2Integer().getFormat(), is(nullValue()));
	}

	@Test
	void decimal2IntegerGetValidatorIsNull() {
		assertThat(new Decimal2Integer().getValidator(), is(nullValue()));
	}

	@Test
	void decimal2IntegerGetFormatPatternIsNotNull() {
		assertThat(new Decimal2Integer().getFormatPattern(), is(notNullValue()));
	}

	@Test
	void decimal2IntegerToDisplayValueFormatsAsInteger() throws ConversionException {
		String result = new Decimal2Integer().toDisplayValue(new Decimal2("1234"));
		assertThat(result, is("1,234"));
	}

	@Test
	void decimal2IntegerFromDisplayValueReturnsDecimal2() throws ConversionException {
		Decimal2 result = new Decimal2Integer().fromDisplayValue("1,234");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2IntegerFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal2Integer().fromDisplayValue("notanumber"));
	}

	// ---- Decimal2IntegerPercentage ----

	@Test
	void decimal2IntegerPercentageGetValueTypeIsDecimal2() {
		assertThat(new Decimal2IntegerPercentage().getValueType().getSimpleName(), is("Decimal2"));
	}

	@Test
	void decimal2IntegerPercentageGetAttributeTypeIsDecimal2() {
		assertThat(new Decimal2IntegerPercentage().getAttributeType(), is(AttributeType.decimal2));
	}

	@Test
	void decimal2IntegerPercentageGetFormatIsNull() {
		assertThat(new Decimal2IntegerPercentage().getFormat(), is(nullValue()));
	}

	@Test
	void decimal2IntegerPercentageGetValidatorIsNull() {
		assertThat(new Decimal2IntegerPercentage().getValidator(), is(nullValue()));
	}

	@Test
	void decimal2IntegerPercentageToDisplayValueAddsPercent() throws ConversionException {
		String result = new Decimal2IntegerPercentage().toDisplayValue(new Decimal2("0.5"));
		assertThat(result, is("50%"));
	}

	@Test
	void decimal2IntegerPercentageFromDisplayValueStripsPercent() throws ConversionException {
		Decimal2 result = new Decimal2IntegerPercentage().fromDisplayValue("50%");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2IntegerPercentageFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal2IntegerPercentage().fromDisplayValue("notanumber%"));
	}

	// ---- Decimal2OneDecimalPlace ----

	@Test
	void decimal2OneDecimalPlaceGetValueTypeIsDecimal2() {
		assertThat(new Decimal2OneDecimalPlace().getValueType().getSimpleName(), is("Decimal2"));
	}

	@Test
	void decimal2OneDecimalPlaceGetAttributeTypeIsDecimal2() {
		assertThat(new Decimal2OneDecimalPlace().getAttributeType(), is(AttributeType.decimal2));
	}

	@Test
	void decimal2OneDecimalPlaceGetFormatIsNull() {
		assertThat(new Decimal2OneDecimalPlace().getFormat(), is(nullValue()));
	}

	@Test
	void decimal2OneDecimalPlaceGetValidatorIsNull() {
		assertThat(new Decimal2OneDecimalPlace().getValidator(), is(nullValue()));
	}

	@Test
	void decimal2OneDecimalPlaceToDisplayValueFormatsOneDecimalPlace() throws ConversionException {
		String result = new Decimal2OneDecimalPlace().toDisplayValue(new Decimal2("1234.5"));
		assertThat(result, is("1,234.5"));
	}

	@Test
	void decimal2OneDecimalPlaceFromDisplayValueReturnsDecimal2() throws ConversionException {
		Decimal2 result = new Decimal2OneDecimalPlace().fromDisplayValue("1,234.5");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2OneDecimalPlaceFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal2OneDecimalPlace().fromDisplayValue("notanumber"));
	}

	// ---- Decimal5Integer ----

	@Test
	void decimal5IntegerGetValueTypeIsDecimal5() {
		assertThat(new Decimal5Integer().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5IntegerGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5Integer().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5IntegerGetFormatIsNull() {
		assertThat(new Decimal5Integer().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5IntegerGetValidatorIsNull() {
		assertThat(new Decimal5Integer().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5IntegerToDisplayValueFormatsAsInteger() throws ConversionException {
		String result = new Decimal5Integer().toDisplayValue(new Decimal5("9876"));
		assertThat(result, is("9,876"));
	}

	@Test
	void decimal5IntegerFromDisplayValueReturnsDecimal5() throws ConversionException {
		Decimal5 result = new Decimal5Integer().fromDisplayValue("9,876");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5IntegerFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5Integer().fromDisplayValue("notanumber"));
	}

	// ---- Decimal5IntegerPercentage ----

	@Test
	void decimal5IntegerPercentageGetValueTypeIsDecimal5() {
		assertThat(new Decimal5IntegerPercentage().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5IntegerPercentageGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5IntegerPercentage().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5IntegerPercentageGetFormatIsNull() {
		assertThat(new Decimal5IntegerPercentage().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5IntegerPercentageGetValidatorIsNull() {
		assertThat(new Decimal5IntegerPercentage().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5IntegerPercentageToDisplayValueAddsPercent() throws ConversionException {
		String result = new Decimal5IntegerPercentage().toDisplayValue(new Decimal5("0.5"));
		assertThat(result, is("50%"));
	}

	@Test
	void decimal5IntegerPercentageFromDisplayValueStripsPercent() throws ConversionException {
		Decimal5 result = new Decimal5IntegerPercentage().fromDisplayValue("50%");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5IntegerPercentageFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5IntegerPercentage().fromDisplayValue("notanumber%"));
	}

	// ---- Decimal5OneDecimalPlace ----

	@Test
	void decimal5OneDecimalPlaceGetValueTypeIsDecimal5() {
		assertThat(new Decimal5OneDecimalPlace().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5OneDecimalPlaceGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5OneDecimalPlace().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5OneDecimalPlaceGetFormatIsNull() {
		assertThat(new Decimal5OneDecimalPlace().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5OneDecimalPlaceGetValidatorIsNull() {
		assertThat(new Decimal5OneDecimalPlace().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5OneDecimalPlaceToDisplayValueFormatsOneDecimalPlace() throws ConversionException {
		String result = new Decimal5OneDecimalPlace().toDisplayValue(new Decimal5("9876.5"));
		assertThat(result, is("9,876.5"));
	}

	@Test
	void decimal5OneDecimalPlaceFromDisplayValueReturnsDecimal5() throws ConversionException {
		Decimal5 result = new Decimal5OneDecimalPlace().fromDisplayValue("9,876.5");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5OneDecimalPlaceFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5OneDecimalPlace().fromDisplayValue("notanumber"));
	}

	// ---- Decimal5TwoDecimalPlaces ----

	@Test
	void decimal5TwoDecimalPlacesGetValueTypeIsDecimal5() {
		assertThat(new Decimal5TwoDecimalPlaces().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5TwoDecimalPlacesGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5TwoDecimalPlaces().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5TwoDecimalPlacesGetFormatIsNull() {
		assertThat(new Decimal5TwoDecimalPlaces().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5TwoDecimalPlacesGetValidatorIsNull() {
		assertThat(new Decimal5TwoDecimalPlaces().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5TwoDecimalPlacesToDisplayValueFormatsTwoDecimalPlaces() throws ConversionException {
		String result = new Decimal5TwoDecimalPlaces().toDisplayValue(new Decimal5("1234.56"));
		assertThat(result, is("1,234.56"));
	}

	@Test
	void decimal5TwoDecimalPlacesFromDisplayValueReturnsDecimal5() throws ConversionException {
		Decimal5 result = new Decimal5TwoDecimalPlaces().fromDisplayValue("1,234.56");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5TwoDecimalPlacesFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5TwoDecimalPlaces().fromDisplayValue("notanumber"));
	}

	// ---- Decimal5TwoDecimalPlacesPercentage ----

	@Test
	void decimal5TwoDecimalPlacesPercentageGetValueTypeIsDecimal5() {
		assertThat(new Decimal5TwoDecimalPlacesPercentage().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentageGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5TwoDecimalPlacesPercentage().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentageGetFormatIsNull() {
		assertThat(new Decimal5TwoDecimalPlacesPercentage().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentageGetValidatorIsNull() {
		assertThat(new Decimal5TwoDecimalPlacesPercentage().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentageToDisplayValueAddsPercent() throws ConversionException {
		String result = new Decimal5TwoDecimalPlacesPercentage().toDisplayValue(new Decimal5("0.5"));
		assertThat(result, is("50.00%"));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentageFromDisplayValueStripsPercent() throws ConversionException {
		Decimal5 result = new Decimal5TwoDecimalPlacesPercentage().fromDisplayValue("50.00%");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentageFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5TwoDecimalPlacesPercentage().fromDisplayValue("notanumber%"));
	}

	// ---- Decimal5TimeDuration ----

	@Test
	void decimal5TimeDurationGetValueTypeIsDecimal5() {
		assertThat(new Decimal5TimeDuration().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5TimeDurationGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5TimeDuration().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5TimeDurationGetFormatIsNull() {
		assertThat(new Decimal5TimeDuration().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5TimeDurationGetValidatorIsNull() {
		assertThat(new Decimal5TimeDuration().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5TimeDurationGetFormatPatternIsNull() {
		assertThat(new Decimal5TimeDuration().getFormatPattern(), is(nullValue()));
	}

	@Test
	void decimal5TimeDurationFromDisplayValuePositiveHoursAndMinutes() throws ConversionException {
		Decimal5 result = new Decimal5TimeDuration().fromDisplayValue("2:30");
		assertEquals(0, result.compareTo(new Decimal5("2.5")));
	}

	@Test
	void decimal5TimeDurationFromDisplayValueNegativeHoursAndMinutes() throws ConversionException {
		Decimal5 result = new Decimal5TimeDuration().fromDisplayValue("-2:30");
		assertEquals(0, result.compareTo(new Decimal5("-2.5")));
	}

	@Test
	void decimal5TimeDurationFromDisplayValueNoColonThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5TimeDuration().fromDisplayValue("invalid"));
	}

	@Test
	void decimal5TimeDurationFromDisplayValueInvalidNumberThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5TimeDuration().fromDisplayValue("abc:30"));
	}

	@Test
	void decimal5TimeDurationToDisplayValueFormatsAsHoursColon() throws ConversionException {
		String result = new Decimal5TimeDuration().toDisplayValue(new Decimal5("2.5"));
		assertThat(result, is("2:30"));
	}
}
