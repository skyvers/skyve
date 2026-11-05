package org.skyve.domain.types.converters.integer;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.messages.ConversionException;
import org.skyve.metadata.model.Attribute.AttributeType;

class SimplePercentageTest {
	private static Stream<String> invalidDisplayValues() {
		return Stream.of("notANumber%", "99999999999%", "-99999999999%");
	}

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
		assertAll(invalidDisplayValues().map(invalidDisplayValue -> (Executable) () -> assertThrows(ConversionException.class,
				() -> converter.fromDisplayValue(invalidDisplayValue),
				invalidDisplayValue)));
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
