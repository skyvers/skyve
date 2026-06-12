package org.skyve.domain.types.converters.integer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;
import org.skyve.domain.messages.ConversionException;
import org.skyve.metadata.model.Attribute.AttributeType;

@RunWith(Parameterized.class)
public class SimplePercentageTest {
	@Parameters(name = "{0}")
	public static Collection<Object[]> invalidDisplayValues() {
		return Arrays.asList(new Object[][] {
				{"notANumber%"},
				{"99999999999%"},
				{"-99999999999%"}
		});
	}

	@Parameter
	public String invalidDisplayValue;

	@Test
	@SuppressWarnings("static-method")
	public void toDisplayValueAppendsPercentSign() throws ConversionException {
		SimplePercentage converter = new SimplePercentage();
		assertEquals("42%", converter.toDisplayValue(Integer.valueOf(42)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromDisplayValueStripsPercentSign() throws ConversionException {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(Integer.valueOf(42), converter.fromDisplayValue("42%"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromDisplayValueWithNoPercentSign() throws ConversionException {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(Integer.valueOf(100), converter.fromDisplayValue("100"));
	}

	public void fromDisplayValueInvalidThrowsConversionException() {
		SimplePercentage converter = new SimplePercentage();
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue(invalidDisplayValue));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getValueTypeReturnsIntegerClass() {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(Integer.class, converter.getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getAttributeTypeReturnsInteger() {
		SimplePercentage converter = new SimplePercentage();
		assertEquals(AttributeType.integer, converter.getAttributeType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getFormatReturnsNull() {
		SimplePercentage converter = new SimplePercentage();
		assertNull(converter.getFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getValidatorReturnsNull() {
		SimplePercentage converter = new SimplePercentage();
		assertNull(converter.getValidator());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getFormatPatternReturnsNull() {
		SimplePercentage converter = new SimplePercentage();
		assertNull(converter.getFormatPattern());
	}
}
