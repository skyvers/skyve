package org.skyve.domain.types.converters.decimal.currency;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.metadata.model.Attribute.AttributeType;

class Decimal2DollarsAndCentsAbsoluteTest {

	private Decimal2DollarsAndCentsAbsolute formatter;

	@BeforeEach
	void before() {
		formatter = new Decimal2DollarsAndCentsAbsolute();
	}

	/**
	 * Tests displaying of a positive value.
	 */
	@Test
	void testPositiveToDisplayValue() throws Exception {
		assertThat(formatter.toDisplayValue(new Decimal2(0.00)), is("0.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(123456789.00)), is("123,456,789.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(+2.5555)), is("2.56"));
	}

	/**
	 * Tests displaying of a negative value.
	 */
	@Test
	void testNegativeToDisplayValue() throws Exception {
		assertThat(formatter.toDisplayValue(new Decimal2(-0.00)), is("0.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(-123456789.00)), is("123,456,789.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(-2.5555)), is("2.56"));
	}

	@Test
	void fromDisplayValuePositiveValue() throws Exception {
		Decimal2 result = formatter.fromDisplayValue("1,234.56");
		assertThat(result, is(new Decimal2("1234.56")));
	}

	@Test
	void fromDisplayValueWithDollarPrefix() throws Exception {
		Decimal2 result = formatter.fromDisplayValue("$1,234.56");
		assertThat(result, is(new Decimal2("1234.56")));
	}

	@Test
	void fromDisplayValueZero() throws Exception {
		Decimal2 result = formatter.fromDisplayValue("0.00");
		assertThat(result, is(new Decimal2("0.00")));
	}

	@Test
	@SuppressWarnings("static-method")
	void getValueTypeReturnsDecimal2() {
		assertEquals(Decimal2.class, new Decimal2DollarsAndCentsAbsolute().getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getAttributeTypeReturnsDecimal2() {
		assertEquals(AttributeType.decimal2, new Decimal2DollarsAndCentsAbsolute().getAttributeType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatReturnsNull() {
		assertNull(new Decimal2DollarsAndCentsAbsolute().getFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	void getValidatorReturnsNull() {
		assertNull(new Decimal2DollarsAndCentsAbsolute().getValidator());
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatPatternReturnsPattern() {
		assertEquals(Decimal2DollarsAndCentsAbsolute.PATTERN, new Decimal2DollarsAndCentsAbsolute().getFormatPattern());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueInvalidThrowsConversionException() {
		assertThrows(ConversionException.class, () -> new Decimal2DollarsAndCentsAbsolute().fromDisplayValue("not_a_number"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayValueNullThrows() {
		assertThrows(ConversionException.class, () -> new Decimal2DollarsAndCentsAbsolute().toDisplayValue(null));
	}
}
