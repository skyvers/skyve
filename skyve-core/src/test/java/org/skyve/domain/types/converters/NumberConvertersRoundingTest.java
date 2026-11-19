package org.skyve.domain.types.converters;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.metadata.ConverterName;

/**
 * The number converters (every number {@link ConverterName} but the duration, which
 * {@code Decimal5TimeDurationRoundingTest} covers) against an independent reference: the stored decimal (times 100 for a
 * percentage, its magnitude for the absolute converter) rounded once, HALF_UP, to the places shown, grouped in thousands
 * by hand, with a {@code -} for a negative unless it rounds to zero, and a {@code %} for a percentage. No DecimalFormat.
 * Also what each converter reads, and both round trips.
 */
@SuppressWarnings({"rawtypes", "unchecked"}) // converters are looked up by name
class NumberConvertersRoundingTest {
	/**
	 * A converter's display rule.
	 *
	 * @param name	the converter
	 * @param scale	the stored scale (Decimal2: 2, Decimal5: 5, Decimal10: 10)
	 * @param places	the places shown (of the percentage, for a percentage)
	 * @param percent	shows the value times 100 with a %
	 * @param absolute	shows the magnitude
	 * @param factory	makes the stored decimal
	 */
	private record Spec(ConverterName name, int scale, int places, boolean percent, boolean absolute, Function<BigDecimal, Decimal> factory) {
		Converter converter() {
			return name.getConverter();
		}
	}

	private static final Function<BigDecimal, Decimal> D2 = Decimal2::new;
	private static final Function<BigDecimal, Decimal> D5 = Decimal5::new;
	private static final Function<BigDecimal, Decimal> D10 = Decimal10::new;

	private static final List<Spec> DECIMALS = List.of(
		new Spec(ConverterName.Decimal2DollarsAndCents, 2, 2, false, false, D2),
		new Spec(ConverterName.Decimal2DollarsAndCentsAbsolute, 2, 2, false, true, D2),
		new Spec(ConverterName.Decimal2Integer, 2, 0, false, false, D2),
		new Spec(ConverterName.Decimal2IntegerPercentage, 2, 0, true, false, D2),
		new Spec(ConverterName.Decimal2OneDecimalPlace, 2, 1, false, false, D2),
		new Spec(ConverterName.Decimal5Integer, 5, 0, false, false, D5),
		new Spec(ConverterName.Decimal5IntegerPercentage, 5, 0, true, false, D5),
		new Spec(ConverterName.Decimal5DollarsAndCents, 5, 2, false, false, D5),
		new Spec(ConverterName.Decimal5OneDecimalPlace, 5, 1, false, false, D5),
		new Spec(ConverterName.Decimal5TwoDecimalPlaces, 5, 2, false, false, D5),
		new Spec(ConverterName.Decimal5TwoDecimalPlacesPercentage, 5, 2, true, false, D5),
		new Spec(ConverterName.Decimal10Integer, 10, 0, false, false, D10),
		new Spec(ConverterName.Decimal10IntegerPercentage, 10, 0, true, false, D10),
		new Spec(ConverterName.Decimal10DollarsAndCents, 10, 2, false, false, D10),
		new Spec(ConverterName.Decimal10OneDecimalPlace, 10, 1, false, false, D10),
		new Spec(ConverterName.Decimal10TwoDecimalPlaces, 10, 2, false, false, D10),
		new Spec(ConverterName.Decimal10TwoDecimalPlacesPercentage, 10, 2, true, false, D10));

	/** Returns the converter, raw, to call with any value. */
	private static Converter converter(ConverterName name) {
		return name.getConverter();
	}

	/** Groups the digits of a whole number in thousands with commas. */
	private static String group(String digits) {
		StringBuilder result = new StringBuilder(digits);
		for (int i = digits.length() - 3; i > 0; i -= 3) {
			result.insert(i, ',');
		}
		return result.toString();
	}

	/** What a decimal converter shows for a stored value. */
	static String reference(Spec spec, BigDecimal stored) {
		BigDecimal value = spec.percent() ? stored.movePointRight(2) : stored;
		if (spec.absolute()) {
			value = value.abs();
		}
		BigDecimal rounded = value.setScale(spec.places(), RoundingMode.HALF_UP);
		String plain = rounded.abs().toPlainString();
		int point = plain.indexOf('.');
		String whole = (point < 0) ? plain : plain.substring(0, point);
		String fraction = (point < 0) ? "" : plain.substring(point);
		return ((rounded.signum() < 0) ? "-" : "") + group(whole) + fraction + (spec.percent() ? "%" : "");
	}

	/** What a decimal converter reads back from what it shows: the value rounded to the places shown. */
	static BigDecimal shownValue(Spec spec, BigDecimal stored) {
		BigDecimal value = spec.absolute() ? stored.abs() : stored;
		int places = spec.percent() ? spec.places() + 2 : spec.places();
		return value.setScale(places, RoundingMode.HALF_UP);
	}

	/** Collects mismatches: their count and the first ten. */
	private static final class Mismatches {
		long count;
		final List<String> first = new ArrayList<>(10);

		void add(String mismatch) {
			count++;
			if (first.size() < 10) {
				first.add(mismatch);
			}
		}

		void assertNone(String what) {
			assertEquals(0L, count, () -> what + ": " + count + " mismatch(es), the first " + first);
		}
	}

	private static void check(Spec spec, BigDecimal stored, Mismatches mismatches, boolean roundTrips) {
		Converter converter = spec.converter();
		Decimal value = spec.factory().apply(stored);
		BigDecimal exact = value.bigDecimalValue();
		String expected = reference(spec, exact);
		try {
			String shown = converter.toDisplayValue(value);
			if (! expected.equals(shown)) {
				mismatches.add(spec.name() + " " + exact.toPlainString() + " shows " + shown + " not " + expected);
				return;
			}
			if (roundTrips) {
				BigDecimal read = ((Decimal) converter.fromDisplayValue(shown)).bigDecimalValue();
				BigDecimal wanted = shownValue(spec, exact);
				String again = converter.toDisplayValue(converter.fromDisplayValue(shown));
				if ((read.compareTo(wanted) != 0) || (! shown.equals(again))) {
					mismatches.add(spec.name() + " " + exact.toPlainString() + " shows " + shown + ", which reads " + read.toPlainString() +
									" (not " + wanted.toPlainString() + ") and shows " + again);
				}
			}
		}
		catch (Exception e) {
			mismatches.add(spec.name() + " " + exact.toPlainString() + " threw " + e);
		}
	}

	/**
	 * Every Decimal2 within +/-2,000.00 (400,001 values) in each Decimal2 converter, and both round trips. The range is
	 * narrowed from +/-100,000.00 (about 200 s: a DecimalFormat per value) to keep the test to seconds: it crosses the
	 * first grouping, rounding repeats beyond it, and the half-way and random tests cover larger values.
	 */
	@Test
	@SuppressWarnings("static-method")
	void everyDecimal2WithinTwoThousand() {
		Mismatches mismatches = new Mismatches();
		for (Spec spec : DECIMALS) {
			if (spec.scale() != 2) {
				continue;
			}
			for (long units = -200_000L; units <= 200_000L; units++) {
				check(spec, BigDecimal.valueOf(units, 2), mismatches, true);
			}
		}
		mismatches.assertNone("Decimal2 within +/-2,000.00");
	}

	/**
	 * The values half way between two shown values, and their neighbours a stored unit either side, from the smallest the
	 * scale holds to 10^12, in every Decimal5 and Decimal10 converter, of both signs.
	 */
	@Test
	@SuppressWarnings("static-method")
	void halfWayPointsAndTheirNeighbours() {
		Mismatches mismatches = new Mismatches();
		for (Spec spec : DECIMALS) {
			int shownScale = spec.percent() ? spec.places() + 2 : spec.places();
			BigDecimal half = BigDecimal.ONE.movePointLeft(shownScale + 1).multiply(BigDecimal.valueOf(5));
			BigDecimal unit = BigDecimal.ONE.movePointLeft(spec.scale());
			for (int exponent = -spec.scale(); exponent <= 12; exponent++) {
				for (int digit : new int[] {1, 3, 7, 9}) {
					BigDecimal base = BigDecimal.valueOf(digit).movePointRight(exponent).setScale(shownScale, RoundingMode.DOWN);
					BigDecimal halfWay = base.add(half);
					if (halfWay.scale() > spec.scale()) {
						continue; // the stored scale cannot hold it
					}
					for (BigDecimal value : new BigDecimal[] {halfWay, halfWay.subtract(unit), halfWay.add(unit)}) {
						check(spec, value, mismatches, true);
						check(spec, value.negate(), mismatches, true);
					}
				}
			}
		}
		mismatches.assertNone("half-way points");
	}

	/**
	 * A million values (seeded), of every size the types hold, spread over the Decimal5 and Decimal10 converters; and each
	 * type's largest and smallest values.
	 */
	@Test
	@SuppressWarnings("static-method")
	void aMillionRandomValuesAndTheLimits() {
		Random random = new Random(20261117L);
		Mismatches mismatches = new Mismatches();
		List<Spec> specs = DECIMALS.stream().filter(s -> s.scale() > 2).toList();
		for (int i = 0; i < 1_000_000; i++) {
			Spec spec = specs.get(i % specs.size());
			int digits = 1 + random.nextInt(spec.scale() + 15);
			long unscaled = random.nextLong(BigDecimal.TEN.pow(Math.min(digits, 18)).longValueExact());
			check(spec, BigDecimal.valueOf(random.nextBoolean() ? unscaled : -unscaled, spec.scale()), mismatches, true);
		}
		for (Spec spec : DECIMALS) {
			BigDecimal max = BigDecimal.ONE.movePointRight(spec.scale() == 10 ? 18 : 15).subtract(BigDecimal.ONE.movePointLeft(spec.scale()));
			check(spec, max, mismatches, false);
			check(spec, max.negate(), mismatches, false);
		}
		mismatches.assertNone("random values and limits");
	}

	private static Executable shows(ConverterName name, Function<String, Object> factory, String stored, String expected) {
		return () -> assertEquals(expected, converter(name).toDisplayValue(factory.apply(stored)), name + " " + stored);
	}

	@Test
	@SuppressWarnings("static-method")
	void namedDecimalsRoundHalfUpFromTheDecimal() {
		assertAll(shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "1.005", "1.01"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "1.015", "1.02"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "8.345", "8.35"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "1.255", "1.26"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "2.345", "2.35"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "-2.345", "-2.35"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "0.005", "0.01"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "-0.005", "-0.01"),
					shows(ConverterName.Decimal5TwoDecimalPlacesPercentage, Decimal5::new, "0.12345", "12.35%"),
					shows(ConverterName.Decimal10TwoDecimalPlaces, Decimal10::new, "1234567890123.455", "1,234,567,890,123.46"));
	}

	@Test
	@SuppressWarnings("static-method")
	void aValueThatRoundsToZeroShowsNoSign() {
		assertAll(shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "-0.001", "0.00"),
					shows(ConverterName.Decimal5TwoDecimalPlaces, Decimal5::new, "-0.00499", "0.00"),
					shows(ConverterName.Decimal5Integer, Decimal5::new, "-0.4", "0"),
					shows(ConverterName.Decimal5OneDecimalPlace, Decimal5::new, "-0.04999", "0.0"),
					shows(ConverterName.Decimal5IntegerPercentage, Decimal5::new, "-0.00004", "0%"),
					shows(ConverterName.Decimal2DollarsAndCentsAbsolute, Decimal2::new, "-0.00", "0.00"));
	}

	@Test
	@SuppressWarnings("static-method")
	void percentagesOverAThousandAreGrouped() {
		assertAll(shows(ConverterName.Decimal2IntegerPercentage, Decimal2::new, "10", "1,000%"),
					shows(ConverterName.Decimal2IntegerPercentage, Decimal2::new, "12.5", "1,250%"),
					shows(ConverterName.Decimal5TwoDecimalPlacesPercentage, Decimal5::new, "12.345", "1,234.50%"),
					shows(ConverterName.Decimal10IntegerPercentage, Decimal10::new, "-1234.56789", "-123,457%"));
	}

	@Test
	@SuppressWarnings("static-method")
	void theAbsoluteConverterShowsTheMagnitude() {
		assertAll(shows(ConverterName.Decimal2DollarsAndCentsAbsolute, Decimal2::new, "-7.5", "7.50"),
					shows(ConverterName.Decimal2DollarsAndCentsAbsolute, Decimal2::new, "7.5", "7.50"),
					shows(ConverterName.Decimal2DollarsAndCentsAbsolute, Decimal2::new, "-1234567.89", "1,234,567.89"));
	}

	private static Executable reads(ConverterName name, String typed, String expected) {
		return () -> {
			Object value = converter(name).fromDisplayValue(typed);
			BigDecimal read = (value instanceof Decimal decimal) ? decimal.bigDecimalValue() : new BigDecimal(value.toString());
			assertEquals(0, new BigDecimal(expected).compareTo(read), name + " '" + typed + "' reads " + read);
		};
	}

	private static Executable refuses(ConverterName name, String typed) {
		Converter converter = converter(name);
		return () -> assertThrows(ConversionException.class, () -> converter.fromDisplayValue(typed), name + " '" + typed + "'");
	}

	@Test
	@SuppressWarnings("static-method")
	void decimalsReadANumberAndNothingElse() {
		ConverterName n = ConverterName.Decimal5TwoDecimalPlaces;
		assertAll(reads(n, "1,234.56", "1234.56"),
					reads(n, "1234.56", "1234.56"),
					reads(n, "-1,234.56", "-1234.56"),
					reads(n, " 12 ", "12"), // trimmed
					reads(n, "1,23,4", "1234"), // grouping is read leniently
					reads(n, "2.34567", "2.34567"), // more places than shown: kept to the type's scale
					reads(n, "2.345678", "2.34568"), // and HALF_UP beyond it
					reads(n, "1.", "1"),
					reads(n, ".5", "0.5"),
					reads(ConverterName.Decimal2OneDecimalPlace, "2.345", "2.35")); // Decimal2 keeps 2 places
	}

	@Test
	@SuppressWarnings("static-method")
	void decimalsRefuseAnythingElse() {
		ConverterName n = ConverterName.Decimal5TwoDecimalPlaces;
		assertAll(refuses(n, ""),
					refuses(n, "abc"),
					refuses(n, "12abc"), // text after the number
					refuses(n, "1E3"), // no exponent, in any case
					refuses(n, "1e3"),
					refuses(n, "1,"), // a grouping separator only between digits
					refuses(n, "-1,"),
					refuses(n, ",1"),
					refuses(n, "-,5"),
					refuses(n, "1,.5"),
					refuses(n, "1.5,0"),
					refuses(n, "\u0661\u0662"), // ASCII digits only (Arabic-Indic 12)
					refuses(n, "\uFF11\uFF12"), // (fullwidth 12)
					refuses(n, "+5"), // no + sign
					refuses(n, "$5"), // no $ but for dollars
					refuses(n, "5%")); // no % but for percentages
	}

	@Test
	@SuppressWarnings("static-method")
	void dollarsReadADollarSignBeforeTheNumberOrAfterItsMinus() {
		ConverterName n = ConverterName.Decimal2DollarsAndCents;
		assertAll(reads(n, "$1,234.56", "1234.56"),
					reads(n, "1,234.56", "1234.56"),
					reads(n, "$-1.00", "-1"),
					reads(n, "-$1.00", "-1"),
					reads(n, "$ 1.00", "1"),
					reads(n, "0.005", "0.01"),
					refuses(n, "1$2"),
					refuses(n, "1.00$"),
					refuses(n, "$"),
					refuses(n, "$$1"),
					refuses(n, "$1,"),
					refuses(n, "-$,5"),
					reads(ConverterName.Decimal2DollarsAndCentsAbsolute, "7.50", "7.5"),
					reads(ConverterName.Decimal2DollarsAndCentsAbsolute, "-7.50", "-7.5")); // read as typed, though shown 7.50
	}

	@Test
	@SuppressWarnings("static-method")
	void percentagesReadAPercentSignAtTheEndOnly() {
		assertAll(reads(ConverterName.Decimal2IntegerPercentage, "12%", "0.12"),
					reads(ConverterName.Decimal2IntegerPercentage, "12", "0.12"),
					reads(ConverterName.Decimal2IntegerPercentage, "1,250%", "12.5"),
					reads(ConverterName.Decimal2IntegerPercentage, "12.5%", "0.13"), // rounded to the places shown
					reads(ConverterName.Decimal5TwoDecimalPlacesPercentage, "12.345%", "0.1235"),
					reads(ConverterName.Decimal5TwoDecimalPlacesPercentage, " -3.25 % ", "-0.0325"),
					refuses(ConverterName.Decimal2IntegerPercentage, "12%3"),
					refuses(ConverterName.Decimal2IntegerPercentage, "%12"),
					refuses(ConverterName.Decimal2IntegerPercentage, "12%%"),
					refuses(ConverterName.Decimal2IntegerPercentage, "1,%"),
					refuses(ConverterName.Decimal2IntegerPercentage, ",5%"),
					refuses(ConverterName.Decimal2IntegerPercentage, "1e3%"),
					reads(ConverterName.SimplePercentage, "12%", "12"),
					reads(ConverterName.SimplePercentage, "-5", "-5"),
					reads(ConverterName.SimplePercentage, " 1000 % ", "1000"),
					refuses(ConverterName.SimplePercentage, "12%3"),
					refuses(ConverterName.SimplePercentage, "1,000%"),
					refuses(ConverterName.SimplePercentage, "12.5%"),
					refuses(ConverterName.SimplePercentage, "\u0661\u0662%"), // ASCII digits only
					refuses(ConverterName.SimplePercentage, "1e3%"),
					refuses(ConverterName.SimplePercentage, "99999999999%")); // beyond an int
	}

	@Test
	@SuppressWarnings("static-method")
	void integersReadTheirDigitsWithOptionalGrouping() {
		assertAll(reads(ConverterName.IntegerSeparator, "1,234,567", "1234567"),
					reads(ConverterName.IntegerSeparator, " -42 ", "-42"),
					reads(ConverterName.IntegerSeparator, "+5", "5"),
					reads(ConverterName.IntegerSeparator, "2,147,483,647", "2147483647"),
					refuses(ConverterName.IntegerSeparator, "2,147,483,648"),
					refuses(ConverterName.IntegerSeparator, "1.5"),
					refuses(ConverterName.IntegerSeparator, ""),
					reads(ConverterName.LongIntegerSeparator, "9,223,372,036,854,775,807", "9223372036854775807"),
					reads(ConverterName.LongIntegerSeparator, "-9,223,372,036,854,775,808", "-9223372036854775808"),
					refuses(ConverterName.LongIntegerSeparator, "9,223,372,036,854,775,808"),
					refuses(ConverterName.LongIntegerSeparator, "12abc"),
					refuses(ConverterName.IntegerSeparator, "1,"), // a grouping separator only between digits
					refuses(ConverterName.IntegerSeparator, "-1,"),
					refuses(ConverterName.IntegerSeparator, ",1"),
					refuses(ConverterName.IntegerSeparator, "-,5"),
					refuses(ConverterName.LongIntegerSeparator, "1,"),
					refuses(ConverterName.LongIntegerSeparator, ",1"),
					refuses(ConverterName.IntegerSeparator, "\u0661\u0662"), // ASCII digits only
					refuses(ConverterName.IntegerSeparator, "\uFF11\uFF12"),
					refuses(ConverterName.LongIntegerSeparator, "\u0661\u0662"),
					refuses(ConverterName.IntegerSeparator, "1e3"));
	}

	/** The reference for the whole number converters. */
	private static String referenceInteger(long value, boolean grouped, boolean percent) {
		String digits = Long.toString(Math.abs(value));
		if (value == Long.MIN_VALUE) {
			digits = "9223372036854775808";
		}
		return ((value < 0L) ? "-" : "") + (grouped ? group(digits) : digits) + (percent ? "%" : "");
	}

	@Test
	@SuppressWarnings("static-method")
	void wholeNumbersShowTheirDigitsGrouped() {
		long[] ints = {0L, 1L, -1L, 999L, 1000L, -999L, -1000L, 999_999L, 1_000_000L, -1_000_000L,
						Integer.MAX_VALUE, Integer.MIN_VALUE};
		long[] longs = {0L, 1L, -1L, 999L, 1000L, 999_999L, 1_000_000L, 12_345_678_901L, Long.MAX_VALUE, Long.MIN_VALUE};
		List<Executable> checks = new ArrayList<>();
		for (long v : ints) {
			Integer value = Integer.valueOf((int) v);
			checks.add(() -> assertEquals(referenceInteger(v, true, false), converter(ConverterName.IntegerSeparator).toDisplayValue(value)));
			checks.add(() -> assertEquals(referenceInteger(v, false, true), converter(ConverterName.SimplePercentage).toDisplayValue(value)));
			checks.add(() -> assertEquals(value, converter(ConverterName.IntegerSeparator).fromDisplayValue(referenceInteger(v, true, false))));
			checks.add(() -> assertEquals(value, converter(ConverterName.SimplePercentage).fromDisplayValue(referenceInteger(v, false, true))));
		}
		for (long v : longs) {
			Long value = Long.valueOf(v);
			checks.add(() -> assertEquals(referenceInteger(v, true, false), converter(ConverterName.LongIntegerSeparator).toDisplayValue(value)));
			checks.add(() -> assertEquals(value, converter(ConverterName.LongIntegerSeparator).fromDisplayValue(referenceInteger(v, true, false))));
		}
		assertAll(checks.stream());
	}

	@Test
	@SuppressWarnings("static-method")
	void theLimitsShowAndReadBack() {
		Mismatches mismatches = new Mismatches();
		Stream.of(DECIMALS.toArray(new Spec[0])).forEach(spec -> {
			BigDecimal max = BigDecimal.ONE.movePointRight(spec.scale() == 10 ? 18 : 15).subtract(BigDecimal.ONE.movePointLeft(spec.places()));
			check(spec, max, mismatches, ! spec.percent());
		});
		mismatches.assertNone("limits");
		// rounding carries into a new digit and group
		assertEquals("100,000,000,000,000,000.00", converter(ConverterName.Decimal10TwoDecimalPlaces).toDisplayValue(new Decimal10("99999999999999999.999")));
	}
}
