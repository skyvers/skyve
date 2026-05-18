package org.skyve.domain.types.converters.integer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.metadata.model.Attribute.AttributeType;

class SimplePercentageTest {

	@Test
	@SuppressWarnings("static-method")
	void toDisplayValueAppendsPercentSign() throws ConversionException {
		SimplePercentage converter = new SimplePercentage();
		assertEquals("42%", converter.toDisplayValue(Integer.valueOf(42)));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueStripsPercentSign() throws ConversionException {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(Integer.valueOf(42), converter.fromDisplayValue("42%"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueWithNoPercentSign() throws ConversionException {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(Integer.valueOf(100), converter.fromDisplayValue("100"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueInvalidThrowsConversionException() {
		SimplePercentage converter = new SimplePercentage();
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("notANumber%"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueVeryLargeNumberThrowsConversionException() {
		SimplePercentage converter = new SimplePercentage();
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("99999999999%"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueVerySmallNegativeNumberThrowsConversionException() {
		SimplePercentage converter = new SimplePercentage();
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("-99999999999%"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getValueTypeReturnsIntegerClass() {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(Integer.class, converter.getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getAttributeTypeReturnsInteger() {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(AttributeType.integer, converter.getAttributeType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatReturnsNull() {
		SimplePercentage converter = new SimplePercentage();
		assertNull(converter.getFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	void getValidatorReturnsNull() {
		SimplePercentage converter = new SimplePercentage();
		assertNull(converter.getValidator());
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatPatternReturnsNull() {
		SimplePercentage converter = new SimplePercentage();
		assertNull(converter.getFormatPattern());
	}
}
