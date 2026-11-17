package org.skyve.domain.types.converters;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.util.Locale;
import java.util.Locale.Category;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.formatters.DecimalFormatter;
import org.skyve.metadata.ConverterName;

/**
 * The number converters write and read English numbers whatever the JVM's locale, as the SmartClient and Vue renderers
 * do: in a German JVM (where DecimalFormat would write {@code 1.234,50} and read {@code 1E3} as 1000), they still
 * write {@code 1,234.50} and refuse an exponent.
 */
@SuppressWarnings({"rawtypes", "unchecked"}) // converters are looked up by name
class NumberConvertersLocaleTest {
	@Test
	@SuppressWarnings("static-method")
	void numbersAreEnglishInAGermanJvm() {
		Locale display = Locale.getDefault(Category.DISPLAY);
		Locale format = Locale.getDefault(Category.FORMAT);
		Locale locale = Locale.getDefault();
		Locale.setDefault(Locale.GERMANY);
		try {
			Converter twoPlaces = ConverterName.Decimal5TwoDecimalPlaces.getConverter();
			Converter dollars = ConverterName.Decimal2DollarsAndCents.getConverter();
			Converter percentage = ConverterName.Decimal5TwoDecimalPlacesPercentage.getConverter();
			Converter integer = ConverterName.IntegerSeparator.getConverter();
			Converter longInteger = ConverterName.LongIntegerSeparator.getConverter();
			assertAll(() -> assertEquals("1,234.50", twoPlaces.toDisplayValue(new Decimal5("1234.5"))),
						() -> assertEquals(new Decimal5("1234.5"), twoPlaces.fromDisplayValue("1,234.50")),
						() -> assertThrows(ConversionException.class, () -> twoPlaces.fromDisplayValue("1E3")),
						() -> assertThrows(ConversionException.class, () -> twoPlaces.fromDisplayValue("1e3")),
						() -> assertThrows(ConversionException.class, () -> twoPlaces.fromDisplayValue("1.234,50")),
						() -> assertEquals("-1,234.50", dollars.toDisplayValue(new Decimal2("-1234.5"))),
						() -> assertEquals(new Decimal2("-1234.5"), dollars.fromDisplayValue("-$1,234.50")),
						() -> assertEquals("1,234.50%", percentage.toDisplayValue(new Decimal5("12.345"))),
						() -> assertEquals(new Decimal5("0.125"), percentage.fromDisplayValue("12.5%")),
						() -> assertEquals("1,234,567", integer.toDisplayValue(Integer.valueOf(1_234_567))),
						() -> assertEquals(Integer.valueOf(1_234_567), integer.fromDisplayValue("1,234,567")),
						() -> assertEquals("12,345,678,901", longInteger.toDisplayValue(Long.valueOf(12_345_678_901L))),
						() -> assertEquals("1,234.50", new DecimalFormatter("#,##0.00", false, null).toDisplayValue(new BigDecimal("1234.5"))));
		}
		finally {
			Locale.setDefault(locale);
			Locale.setDefault(Category.DISPLAY, display);
			Locale.setDefault(Category.FORMAT, format);
		}
	}
}
