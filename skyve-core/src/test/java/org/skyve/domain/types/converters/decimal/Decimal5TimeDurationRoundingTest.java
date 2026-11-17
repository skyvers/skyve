package org.skyve.domain.types.converters.decimal;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.formatters.TimeDurationFormatter;

/**
 * Time durations (Decimal5 hours shown as h:mm by {@link Decimal5TimeDuration} and {@link TimeDurationFormatter}): the
 * minutes are rounded once, half up (away from zero), from the exact fraction of an hour; the hours are truncated towards
 * zero; a duration that rounds to no minutes shows 0:00, without a sign. A Decimal5 is a whole number of
 * hundred-thousandths of an hour, so the reference here works in whole numbers.
 */
class Decimal5TimeDurationRoundingTest {
	private static final long UNITS_PER_HOUR = 100_000L;

	private final TimeDurationFormatter formatter = new TimeDurationFormatter();
	private final Decimal5TimeDuration converter = new Decimal5TimeDuration();

	/**
	 * Returns what a duration of a whole number of hundred-thousandths of an hour shows: the minutes (units × 60 / 100000)
	 * rounded once, half up, away from zero, as h:mm with a sign when it is not 0:00.
	 */
	private static String reference(long units) {
		long minutes = ((Math.abs(units) * 60L) + (UNITS_PER_HOUR / 2L)) / UNITS_PER_HOUR;
		return text((units < 0L) ? -minutes : minutes);
	}

	/** Returns h:mm for a whole number of minutes, with a sign when it is negative. */
	private static String text(long minutes) {
		long magnitude = Math.abs(minutes);
		long mm = magnitude % 60L;
		return ((minutes < 0L) ? "-" : "") + (magnitude / 60L) + ((mm < 10L) ? ":0" : ":") + mm;
	}

	private String format(long units) {
		return formatter.toDisplayValue(new Decimal5(BigDecimal.valueOf(units, 5)));
	}

	/**
	 * Fails with the number of mismatches and the first ones.
	 */
	private static void assertNoMismatches(String what, long count, List<String> first) {
		assertEquals(0L, count, () -> what + ": " + count + " mismatch(es), the first " + first);
	}

	/**
	 * Every Decimal5 from -48 to +48 hours (9,600,001 values) shows as the reference does.
	 */
	@Test
	void everyDecimal5WithinTwoDaysShowsTheMinutesRoundedOnce() {
		long count = 0L;
		List<String> first = new ArrayList<>(10);
		for (long units = -48L * UNITS_PER_HOUR; units <= 48L * UNITS_PER_HOUR; units++) {
			String actual = format(units);
			String expected = reference(units);
			if (! expected.equals(actual)) {
				count++;
				if (first.size() < 10) {
					first.add(BigDecimal.valueOf(units, 5) + " shows " + actual + " not " + expected);
				}
			}
		}
		assertNoMismatches("Decimal5 durations within +/-48 hours", count, first);
	}

	/**
	 * Every whole minute from -100:00 to +100:00 reads as minutes / 60 (at Decimal5's scale) and shows as it was typed.
	 */
	@Test
	void everyWholeMinuteWithinAHundredHoursRoundTrips() throws ConversionException {
		long count = 0L;
		List<String> first = new ArrayList<>(10);
		for (long minutes = -6000L; minutes <= 6000L; minutes++) {
			String typed = text(minutes);
			Decimal5 value = converter.fromDisplayValue(typed);
			Decimal5 expected = new Decimal5(BigDecimal.valueOf(minutes).divide(BigDecimal.valueOf(60L), 5, java.math.RoundingMode.HALF_UP));
			String shown = converter.toDisplayValue(value);
			if ((value.compareTo(expected) != 0) || (! typed.equals(shown))) {
				count++;
				if (first.size() < 10) {
					first.add(typed + " reads " + value + " (not " + expected + ") and shows " + shown);
				}
			}
		}
		assertNoMismatches("whole minutes within +/-100 hours", count, first);
	}

	/**
	 * A total of n durations of k whole minutes each (n up to 500, k from 1 to 59), each stored at Decimal5's scale, shows
	 * n × k minutes, as a timesheet's total of its lines should (truncation showed them a minute short).
	 */
	@Test
	void summedWholeMinutesShowTheirTotal() throws ConversionException {
		long count = 0L;
		List<String> first = new ArrayList<>(10);
		for (int k = 1; k <= 59; k++) {
			Decimal5 line = converter.fromDisplayValue("0:" + k);
			Decimal5 total = Decimal5.ZERO;
			for (int n = 1; n <= 500; n++) {
				total = total.add(line);
				String shown = formatter.toDisplayValue(total);
				String expected = text((long) n * k);
				if (! expected.equals(shown)) {
					count++;
					if (first.size() < 10) {
						first.add(n + " x 0:" + k + " = " + total + " shows " + shown + " not " + expected);
					}
				}
			}
		}
		assertNoMismatches("summed whole minutes", count, first);
	}

	private Executable shows(String hours, String expected) {
		return () -> assertEquals(expected, formatter.toDisplayValue(new Decimal5(hours)), hours + " hours");
	}

	@Test
	void namedValuesRoundTheirMinutesOnce() {
		assertAll(shows("0.33333", "0:20"), // 19.9998 minutes
					shows("0.66667", "0:40"), // 40.0002 minutes
					shows("2.49990", "2:30"), // 30 lines of 5 minutes, summed at 5 decimal places
					shows("1.16662", "1:10"), // 9.9972 minutes
					shows("0.49166", "0:29"), // 29.4996 minutes: rounded once, not to 29.50 first
					shows("1.26", "1:16"), // 15.6 minutes
					shows("2.475", "2:29"), // 28.5 minutes, half up
					shows("-0.01", "-0:01"), // -0.6 minutes
					shows("-0.5", "-0:30"),
					shows("0.99999", "1:00"),
					shows("-0.99999", "-1:00"));
	}

	@Test
	void minutesThatRoundToSixtyCarryToTheHours() {
		assertAll(shows("0.99999", "1:00"),
					shows("1.99992", "2:00"), // 59.9952 minutes
					shows("-0.99999", "-1:00"),
					shows("-1.99992", "-2:00"),
					shows("1.99166", "1:59"), // 59.4996 minutes
					shows("-1.99166", "-1:59"));
	}

	@Test
	void noMinutesShowZeroWithoutASign() {
		assertAll(shows("0", "0:00"),
					shows("0.00833", "0:00"), // 0.4998 minutes
					shows("-0.00833", "0:00"), // -0.4998 minutes: no sign on 0:00
					shows("0.00834", "0:01"), // 0.5004 minutes
					shows("-0.00834", "-0:01"));
	}

	@Test
	void largeHours() {
		assertAll(shows("100", "100:00"),
					shows("100.5", "100:30"),
					shows("-123.25", "-123:15"),
					shows("1000.99999", "1001:00"),
					shows("9510", "9510:00"),
					shows("-12345.01667", "-12345:01"));
	}

	private Executable reads(String typed, String hours) {
		return () -> assertEquals(0, new Decimal5(hours).compareTo(converter.fromDisplayValue(typed)), typed);
	}

	@Test
	void readsHoursAColonAndMinutes() {
		assertAll(reads("1:30", "1.5"),
					reads("1:75", "2.25"), // minutes of 60 or more
					reads("-0:30", "-0.5"), // the sign is the hours' text's
					reads("-2:15", "-2.25"),
					reads("0:29.4996", "0.49166"), // decimal minutes
					reads("+1:30", "1.5"),
					reads(" 1 : 30 ", "1.5"), // spaces around each part
					reads(".5:00", "0.5"),
					reads("12345:01", "12345.01667"));
	}

	@Test
	void doesNotReadAnythingElse() {
		assertAll(Arrays.stream(new String[] {"1.05", "130", "", ":", "1:", ":30", "abc:30", "1:xx", "1e2:00", "1:30:00", "1 30"})
						.map(typed -> (Executable) () -> assertThrows(ConversionException.class,
																		() -> converter.fromDisplayValue(typed),
																		"'" + typed + "'")));
	}
}
