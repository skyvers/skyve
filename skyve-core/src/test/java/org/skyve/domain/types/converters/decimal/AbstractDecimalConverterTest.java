package org.skyve.domain.types.converters.decimal;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.decimal.currency.Decimal10DollarsAndCents;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute;
import org.skyve.domain.types.converters.decimal.currency.Decimal5DollarsAndCents;

/** Contract tests for every converter built on {@link AbstractDecimalConverter}. */
@SuppressWarnings("static-method")
class AbstractDecimalConverterTest {
	private static final Map<AbstractDecimalConverter<?>, String> CONVERTER_KEYS = Map.<AbstractDecimalConverter<?>, String> ofEntries(
			Map.entry(new Decimal2Integer(), ConversionException.DECIMAL_2_INTEGER_KEY),
			Map.entry(new Decimal2IntegerPercentage(), ConversionException.DECIMAL_2_INTEGER_PERCENTAGE_KEY),
			Map.entry(new Decimal2OneDecimalPlace(), ConversionException.DECIMAL_2_ONE_DECIMAL_PLACE_KEY),
			Map.entry(new Decimal5Integer(), ConversionException.DECIMAL_5_INTEGER_KEY),
			Map.entry(new Decimal5IntegerPercentage(), ConversionException.DECIMAL_5_INTEGER_PERCENTAGE_KEY),
			Map.entry(new Decimal5OneDecimalPlace(), ConversionException.DECIMAL_5_ONE_DECIMAL_PLACE_KEY),
			Map.entry(new Decimal5TwoDecimalPlaces(), ConversionException.DECIMAL_5_TWO_DECIMAL_PLACES_KEY),
			Map.entry(new Decimal5TwoDecimalPlacesPercentage(), ConversionException.DECIMAL_5_TWO_DECIMAL_PLACES_PERCENTAGE_KEY),
			Map.entry(new Decimal10Integer(), ConversionException.DECIMAL_10_INTEGER_KEY),
			Map.entry(new Decimal10IntegerPercentage(), ConversionException.DECIMAL_10_INTEGER_PERCENTAGE_KEY),
			Map.entry(new Decimal10OneDecimalPlace(), ConversionException.DECIMAL_10_ONE_DECIMAL_PLACE_KEY),
			Map.entry(new Decimal10TwoDecimalPlaces(), ConversionException.DECIMAL_10_TWO_DECIMAL_PLACES_KEY),
			Map.entry(new Decimal10TwoDecimalPlacesPercentage(), ConversionException.DECIMAL_10_TWO_DECIMAL_PLACES_PERCENTAGE_KEY),
			Map.entry(new Decimal2DollarsAndCents(), ConversionException.DECIMAL_2_DOLLARS_AND_CENTS_KEY),
			Map.entry(new Decimal2DollarsAndCentsAbsolute(), ConversionException.DECIMAL_2_DOLLARS_AND_CENTS_ABSOLUTE_KEY),
			Map.entry(new Decimal5DollarsAndCents(), ConversionException.DECIMAL_5_DOLLARS_AND_CENTS_KEY),
			Map.entry(new Decimal10DollarsAndCents(), ConversionException.DECIMAL_10_DOLLARS_AND_CENTS_KEY));

	@Test
	void everyConverterRejectsNonNumbersWithItsOwnKey() {
		DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance();
		List<String> inputs = List.of("not a number", "", symbols.getNaN(), symbols.getInfinity(), "-" + symbols.getInfinity());
		List<Executable> checks = new ArrayList<>();
		CONVERTER_KEYS.forEach((converter, key) -> {
			for (String input : inputs) {
				String name = converter.getClass().getSimpleName() + " parsing '" + input + "'";
				checks.add(() -> {
					ConversionException e = assertThrows(ConversionException.class,
															() -> converter.fromDisplayValue(input),
															name);
					assertEquals("Conversion failed for converter :- " + key, e.getMessage(), name);
				});
			}
		});
		assertAll(checks.stream());
	}

	@Test
	void displayValuesRoundHalfUp() {
		assertAll(
				display(new Decimal2Integer(), new Decimal2("2.5"), "3"),
				display(new Decimal5Integer(), new Decimal5("2.5"), "3"),
				display(new Decimal10Integer(), new Decimal10("2.5"), "3"),
				display(new Decimal2OneDecimalPlace(), new Decimal2("0.25"), "0.3"),
				display(new Decimal5OneDecimalPlace(), new Decimal5("0.25"), "0.3"),
				display(new Decimal10OneDecimalPlace(), new Decimal10("0.25"), "0.3"),
				display(new Decimal5TwoDecimalPlaces(), new Decimal5("0.125"), "0.13"),
				display(new Decimal10TwoDecimalPlaces(), new Decimal10("0.125"), "0.13"),
				display(new Decimal5IntegerPercentage(), new Decimal5("0.125"), "13%"),
				display(new Decimal10IntegerPercentage(), new Decimal10("0.125"), "13%"),
				display(new Decimal5TwoDecimalPlacesPercentage(), new Decimal5("0.12345"), "12.35%"),
				display(new Decimal10TwoDecimalPlacesPercentage(), new Decimal10("0.12345"), "12.35%"),
				display(new Decimal5DollarsAndCents(), new Decimal5("0.125"), "0.13"),
				display(new Decimal10DollarsAndCents(), new Decimal10("0.125"), "0.13"),
				display(new Decimal2DollarsAndCentsAbsolute(), new Decimal2("-1.5"), "1.50"));
	}

	@Test
	void percentageEntryRoundsHalfUpToTheDisplayedPrecision() {
		assertAll(
				parse(new Decimal2IntegerPercentage(), "12.5%", new Decimal2("0.13")),
				parse(new Decimal5IntegerPercentage(), "12.5%", new Decimal5("0.13")),
				parse(new Decimal10IntegerPercentage(), "12.5%", new Decimal10("0.13")),
				parse(new Decimal10IntegerPercentage(), "12.4%", new Decimal10("0.12")),
				parse(new Decimal10IntegerPercentage(), "-12.5%", new Decimal10("-0.13")),
				parse(new Decimal10IntegerPercentage(), "12.5", new Decimal10("0.13")),
				parse(new Decimal5TwoDecimalPlacesPercentage(), "12.345%", new Decimal5("0.1235")),
				parse(new Decimal10TwoDecimalPlacesPercentage(), "12.345%", new Decimal10("0.1235")));
	}

	@Test
	void nonPercentageEntryKeepsThePrecisionOfTheType() {
		assertAll(
				parse(new Decimal5OneDecimalPlace(), "1.23456789012", new Decimal5("1.23457")),
				parse(new Decimal10OneDecimalPlace(), "1.23456789012", new Decimal10("1.2345678901")),
				parse(new Decimal10Integer(), "1.23456789012", new Decimal10("1.2345678901")),
				parse(new Decimal10TwoDecimalPlaces(), "1,234.56789012345", new Decimal10("1234.5678901235")),
				parse(new Decimal10DollarsAndCents(), "$1.23456789012", new Decimal10("1.2345678901")));
	}

	private static <T extends Decimal> Executable display(AbstractDecimalConverter<T> converter, T value, String expected) {
		return () -> assertEquals(expected,
									converter.toDisplayValue(value),
									converter.getClass().getSimpleName() + " displaying " + value);
	}

	private static <T extends Decimal> Executable parse(AbstractDecimalConverter<T> converter, String displayValue, T expected) {
		return () -> assertEquals(expected,
									converter.fromDisplayValue(displayValue),
									converter.getClass().getSimpleName() + " parsing '" + displayValue + "'");
	}
}
