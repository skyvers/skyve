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

public class Decimal2DollarsAndCentsAbsoluteTest {

	private Decimal2DollarsAndCentsAbsolute formatter;

	@BeforeEach
	public void before() {
		formatter = new Decimal2DollarsAndCentsAbsolute();
	}

	/**
	 * Tests displaying of a positive value.
	 */
	@Test
	public void testPositiveToDisplayValue() throws Exception {
		assertThat(formatter.toDisplayValue(new Decimal2(0.00)), is("0.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(123456789.00)), is("123,456,789.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(+2.5555)), is("2.56"));
	}

	/**
	 * Tests displaying of a negative value.
	 */
	@Test
	public void testNegativeToDisplayValue() throws Exception {
		assertThat(formatter.toDisplayValue(new Decimal2(-0.00)), is("0.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(-123456789.00)), is("123,456,789.00"));
		assertThat(formatter.toDisplayValue(new Decimal2(-2.5555)), is("2.56"));
	}

	@Test
	public void fromDisplayValuePositiveValue() throws Exception {
		Decimal2 result = formatter.fromDisplayValue("1,234.56");
		assertThat(result, is(new Decimal2("1234.56")));
	}

	@Test
	public void fromDisplayValueWithDollarPrefix() throws Exception {
		Decimal2 result = formatter.fromDisplayValue("$1,234.56");
		assertThat(result, is(new Decimal2("1234.56")));
	}

	@Test
	public void fromDisplayValueZero() throws Exception {
		Decimal2 result = formatter.fromDisplayValue("0.00");
		assertThat(result, is(new Decimal2("0.00")));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getValueTypeReturnsDecimal2() {
		assertEquals(Decimal2.class, new Decimal2DollarsAndCentsAbsolute().getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getAttributeTypeReturnsDecimal2() {
		assertEquals(AttributeType.decimal2, new Decimal2DollarsAndCentsAbsolute().getAttributeType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getFormatReturnsNull() {
		assertNull(new Decimal2DollarsAndCentsAbsolute().getFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getValidatorReturnsNull() {
		assertNull(new Decimal2DollarsAndCentsAbsolute().getValidator());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getFormatPatternReturnsPattern() {
		assertEquals(Decimal2DollarsAndCentsAbsolute.PATTERN, new Decimal2DollarsAndCentsAbsolute().getFormatPattern());
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromDisplayValueInvalidThrowsConversionException() {
		assertThrows(ConversionException.class, () -> new Decimal2DollarsAndCentsAbsolute().fromDisplayValue("not_a_number"));
	}
}
