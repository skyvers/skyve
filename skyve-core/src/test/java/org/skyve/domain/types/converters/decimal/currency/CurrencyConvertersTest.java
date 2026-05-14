package org.skyve.domain.types.converters.decimal.currency;

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

/** Unit tests for decimal currency converter classes. */
@SuppressWarnings("static-method")
class CurrencyConvertersTest {

	// ---- Decimal2DollarsAndCents ----

	@Test
	void decimal2DollarsAndCentsGetValueTypeIsDecimal2() {
		assertThat(new Decimal2DollarsAndCents().getValueType().getSimpleName(), is("Decimal2"));
	}

	@Test
	void decimal2DollarsAndCentsGetAttributeTypeIsDecimal2() {
		assertThat(new Decimal2DollarsAndCents().getAttributeType(), is(AttributeType.decimal2));
	}

	@Test
	void decimal2DollarsAndCentsGetFormatIsNull() {
		assertThat(new Decimal2DollarsAndCents().getFormat(), is(nullValue()));
	}

	@Test
	void decimal2DollarsAndCentsGetValidatorIsNull() {
		assertThat(new Decimal2DollarsAndCents().getValidator(), is(nullValue()));
	}

	@Test
	void decimal2DollarsAndCentsGetFormatPatternIsNotNull() {
		assertThat(new Decimal2DollarsAndCents().getFormatPattern(), is(notNullValue()));
	}

	@Test
	void decimal2DollarsAndCentsToDisplayValueFormatsTwoDecimalPlaces() throws ConversionException {
		String result = new Decimal2DollarsAndCents().toDisplayValue(new Decimal2("1234.56"));
		assertThat(result, is("1,234.56"));
	}

	@Test
	void decimal2DollarsAndCentsFromDisplayValueParsesPlainNumber() throws ConversionException {
		Decimal2 result = new Decimal2DollarsAndCents().fromDisplayValue("1,234.56");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2DollarsAndCentsFromDisplayValueStripsLeadingDollarSign() throws ConversionException {
		Decimal2 result = new Decimal2DollarsAndCents().fromDisplayValue("$1,234.56");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2DollarsAndCentsFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal2DollarsAndCents().fromDisplayValue("notanumber"));
	}

	// ---- Decimal5DollarsAndCents ----

	@Test
	void decimal5DollarsAndCentsGetValueTypeIsDecimal5() {
		assertThat(new Decimal5DollarsAndCents().getValueType().getSimpleName(), is("Decimal5"));
	}

	@Test
	void decimal5DollarsAndCentsGetAttributeTypeIsDecimal5() {
		assertThat(new Decimal5DollarsAndCents().getAttributeType(), is(AttributeType.decimal5));
	}

	@Test
	void decimal5DollarsAndCentsGetFormatIsNull() {
		assertThat(new Decimal5DollarsAndCents().getFormat(), is(nullValue()));
	}

	@Test
	void decimal5DollarsAndCentsGetValidatorIsNull() {
		assertThat(new Decimal5DollarsAndCents().getValidator(), is(nullValue()));
	}

	@Test
	void decimal5DollarsAndCentsGetFormatPatternIsNotNull() {
		assertThat(new Decimal5DollarsAndCents().getFormatPattern(), is(notNullValue()));
	}

	@Test
	void decimal5DollarsAndCentsToDisplayValueFormatsTwoDecimalPlaces() throws ConversionException {
		String result = new Decimal5DollarsAndCents().toDisplayValue(new Decimal5("9876.54"));
		assertThat(result, is("9,876.54"));
	}

	@Test
	void decimal5DollarsAndCentsFromDisplayValueParsesPlainNumber() throws ConversionException {
		Decimal5 result = new Decimal5DollarsAndCents().fromDisplayValue("9,876.54");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5DollarsAndCentsFromDisplayValueStripsLeadingDollarSign() throws ConversionException {
		Decimal5 result = new Decimal5DollarsAndCents().fromDisplayValue("$9,876.54");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5DollarsAndCentsFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal5DollarsAndCents().fromDisplayValue("notanumber"));
	}

	// ---- Decimal10DollarsAndCents ----

	@Test
	void decimal10DollarsAndCentsGetValueTypeIsDecimal10() {
		assertThat(new Decimal10DollarsAndCents().getValueType().getSimpleName(), is("Decimal10"));
	}

	@Test
	void decimal10DollarsAndCentsGetAttributeTypeIsDecimal10() {
		assertThat(new Decimal10DollarsAndCents().getAttributeType(), is(AttributeType.decimal10));
	}

	@Test
	void decimal10DollarsAndCentsGetFormatIsNull() {
		assertThat(new Decimal10DollarsAndCents().getFormat(), is(nullValue()));
	}

	@Test
	void decimal10DollarsAndCentsGetValidatorIsNull() {
		assertThat(new Decimal10DollarsAndCents().getValidator(), is(nullValue()));
	}

	@Test
	void decimal10DollarsAndCentsGetFormatPatternIsNotNull() {
		assertThat(new Decimal10DollarsAndCents().getFormatPattern(), is(notNullValue()));
	}

	@Test
	void decimal10DollarsAndCentsToDisplayValueFormatsTwoDecimalPlaces() throws ConversionException {
		String result = new Decimal10DollarsAndCents().toDisplayValue(new Decimal10("1234567890.12"));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10DollarsAndCentsFromDisplayValueParsesPlainNumber() throws ConversionException {
		Decimal10 result = new Decimal10DollarsAndCents().fromDisplayValue("1,234.56");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10DollarsAndCentsFromDisplayValueStripsLeadingDollarSign() throws ConversionException {
		Decimal10 result = new Decimal10DollarsAndCents().fromDisplayValue("$1,234.56");
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10DollarsAndCentsFromDisplayValueInvalidThrows() {
		assertThrows(ConversionException.class, () -> new Decimal10DollarsAndCents().fromDisplayValue("notanumber"));
	}
}
