package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDate;
import java.time.Month;
import java.util.Calendar;
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;

@SuppressWarnings({ "static-method", "java:S8692" }) // system clock OK
class TimeTest {

	@Test
	void testAddDays() {
		DateOnly initial = new DateOnly();
		DateOnly newTime = new DateOnly(initial);
		assertThat(initial, is(newTime));
		Time.addDays(newTime, 1);
		long diff = (newTime.getTime() - initial.getTime());
		long days = diff / (24 * 60 * 60 * 1000);
		assertEquals(1L, days);
	}

	@Test
	void testAddHours() {
		DateTime initial = new DateTime();
		DateTime newTime = new DateTime(initial);
		assertThat(initial, is(newTime));
		Time.addHours(newTime, 1);
		long secs = (newTime.getTime() - initial.getTime()) / 1000;
		long hours = secs / 3600;
		assertEquals(1L, hours);
	}

	@Test
	void testAddMinutes() {
		DateTime initial = new DateTime();
		DateTime newTime = new DateTime(initial);
		assertThat(initial, is(newTime));
		Time.addMinutes(newTime, 1);
		long secs = (newTime.getTime() - initial.getTime()) / 1000;
		long mins = secs / 60;
		assertEquals(1L, mins);
	}

	@Test
	void testAddSeconds() {
		DateTime initial = new DateTime();
		DateTime newTime = new DateTime(initial);
		assertThat(initial, is(newTime));
		Time.addSeconds(newTime, 1);
		long secs = (newTime.getTime() - initial.getTime()) / 1000;
		assertEquals(1L, secs);
	}

	// ---- clearTimeComponent ----

	@Test
	void clearTimeComponentZerosTime() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 45);
		Date date = cal.getTime();
		Time.clearTimeComponent(date);
		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(0, result.get(Calendar.HOUR_OF_DAY));
		assertEquals(0, result.get(Calendar.MINUTE));
		assertEquals(0, result.get(Calendar.SECOND));
	}

	// ---- clearSecondAndMillisecondComponent ----

	@Test
	void clearSecondAndMillisecondComponentZerosBoth() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.SECOND, 45);
		cal.set(Calendar.MILLISECOND, 500);
		Date date = cal.getTime();
		Time.clearSecondAndMillisecondComponent(date);
		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(0, result.get(Calendar.SECOND));
		assertEquals(0, result.get(Calendar.MILLISECOND));
	}

	// ---- clearMillisecondComponent ----

	@Test
	void clearMillisecondComponentZerosMillis() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.MILLISECOND, 999);
		Date date = cal.getTime();
		Time.clearMillisecondComponent(date);
		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(0, result.get(Calendar.MILLISECOND));
	}

	// ---- clearDateComponent ----

	@Test
	void clearDateComponentSetsEpoch() {
		Date date = new Date();
		Time.clearDateComponent(date);
		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(1970, result.get(Calendar.YEAR));
	}

	// ---- setTime ----

	@Test
	void setTimeSetsHoursMinutesSeconds() {
		Date date = new Date();
		Time.setTime(date, 9, 30, 15);
		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(9, result.get(Calendar.HOUR_OF_DAY));
		assertEquals(30, result.get(Calendar.MINUTE));
		assertEquals(15, result.get(Calendar.SECOND));
	}

	// ---- setDate ----

	@Test
	void setDateSetsDayMonthYear() {
		Date date = new Date();
		Time.setDate(date, 15, 6, 2020);
		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(15, result.get(Calendar.DAY_OF_MONTH));
		assertEquals(Calendar.JUNE, result.get(Calendar.MONTH));
		assertEquals(2020, result.get(Calendar.YEAR));
	}

	// ---- getYear, getMonthStartingFrom1, getMonthStartingFrom0, getDay ----

	@Test
	void getYearReturnsCorrectYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2022, Calendar.MARCH, 10);
		assertEquals(2022, Time.getYear(cal.getTime()));
	}

	@Test
	void getMonthStartingFrom1ReturnsMarch() {
		Calendar cal = Calendar.getInstance();
		cal.set(2022, Calendar.MARCH, 10);
		assertEquals(3, Time.getMonthStartingFrom1(cal.getTime()));
	}

	@Test
	void getMonthStartingFrom0ReturnsMarch() {
		Calendar cal = Calendar.getInstance();
		cal.set(2022, Calendar.MARCH, 10);
		assertEquals(Calendar.MARCH, Time.getMonthStartingFrom0(cal.getTime()));
	}

	@Test
	void getDayReturnsCorrectDay() {
		Calendar cal = Calendar.getInstance();
		cal.set(2022, Calendar.MARCH, 10);
		assertEquals(10, Time.getDay(cal.getTime()));
	}

	// ---- isLeapYear, getDaysInYear ----

	@Test
	void isLeapYear2000ReturnsTrue() {
		assertTrue(Time.isLeapYear(2000));
	}

	@Test
	void isLeapYear1900ReturnsFalse() {
		assertFalse(Time.isLeapYear(1900));
	}

	@Test
	void getDaysInYearLeapYearReturns366() {
		assertEquals(366, Time.getDaysInYear(2000));
	}

	@Test
	void getDaysInYearNonLeapYearReturns365() {
		assertEquals(365, Time.getDaysInYear(2001));
	}

	// ---- addMonths ----

	@Test
	void addMonthsAddsMonths() {
		Calendar cal = Calendar.getInstance();
		cal.set(2022, Calendar.JANUARY, 15);
		Date date = cal.getTime();
		Time.addMonths(date, 2);
		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(Calendar.MARCH, result.get(Calendar.MONTH));
	}

	// ---- getFinancialYear, getFinancialYearString ----

	@Test
	void getFinancialYearAfterJulyReturnsCurrentYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2022, Calendar.AUGUST, 1);
		assertEquals(2022, Time.getFinancialYear(cal.getTime()));
	}

	@Test
	void getFinancialYearBeforeJulyReturnsPreviousYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.MARCH, 1);
		assertEquals(2022, Time.getFinancialYear(cal.getTime()));
	}

	@Test
	void getFinancialYearStringReturnsNonNull() {
		assertNotNull(Time.getFinancialYearString(new Date()));
	}

	// ---- numberOfHoursBetween ----

	@Test
	void numberOfHoursBetweenTwoHoursReturns2() {
		TimeOnly start = new TimeOnly();
		Time.setTime(start, 8, 0, 0);
		TimeOnly end = new TimeOnly();
		Time.setTime(end, 10, 0, 0);
		assertEquals(0, Time.numberOfHoursBetween(start, end).compareTo(new org.skyve.domain.types.Decimal5("2")));
	}

	// ---- numberOfDaysBetween, numberOfDaysInRange ----

	@Test
	void numberOfDaysBetweenTwoDatesReturns1() {
		Calendar start = Calendar.getInstance();
		start.set(2022, Calendar.JANUARY, 1);
		Calendar end = Calendar.getInstance();
		end.set(2022, Calendar.JANUARY, 2);
		assertEquals(1, Time.numberOfDaysBetween(start.getTime(), end.getTime()));
	}

	@Test
	void numberOfDaysInRangeTwoDatesReturns2() {
		Calendar start = Calendar.getInstance();
		start.set(2022, Calendar.JANUARY, 1);
		Calendar end = Calendar.getInstance();
		end.set(2022, Calendar.JANUARY, 2);
		assertEquals(2, Time.numberOfDaysInRange(start.getTime(), end.getTime()));
	}

	// ---- findNextDayOfWeek ----

	@Test
	void findNextDayOfWeekReturnsNonNull() {
		Date result = Time.findNextDayOfWeek(new Date(), Calendar.MONDAY);
		assertNotNull(result);
	}

	// ---- ensureWorkDay ----

	@Test
	void ensureWorkDayReturnsNonNull() {
		assertNotNull(Time.ensureWorkDay(new Date()));
	}

	// ---- daysBetweenDescription ----

	@Test
	void daysBetweenDescriptionSameDayReturnsToday() {
		Date now = new Date();
		assertThat(Time.daysBetweenDescription(now, now), is(notNullValue()));
	}

	// ---- addDaysToNew, addMonthsToNew, addYearsToNew ----

	@Test
	void addDaysToNewReturnsNewDateOnly() {
		DateOnly result = Time.addDaysToNew(new Date(), 5);
		assertNotNull(result);
	}

	@Test
	void addMonthsToNewReturnsNewDateOnly() {
		DateOnly result = Time.addMonthsToNew(new Date(), 1);
		assertNotNull(result);
	}

	@Test
	void addYearsToNewReturnsNewDateOnly() {
		DateOnly result = Time.addYearsToNew(new Date(), 1);
		assertNotNull(result);
	}

	// ---- asDateOnly ----

	@Test
	void asDateOnlyConvertsLocalDate() {
		LocalDate localDate = LocalDate.of(2023, Month.JUNE, 15);
		DateOnly result = Time.asDateOnly(localDate);
		assertNotNull(result);
		assertEquals(2023, Time.getYear(result));
		assertEquals(6, Time.getMonthStartingFrom1(result));
		assertEquals(15, Time.getDay(result));
	}

	// ---- asLocalDate ----

	@Test
	void asLocalDateConvertsDateToLocalDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 0);
		LocalDate result = Time.asLocalDate(cal.getTime());
		assertNotNull(result);
		assertEquals(2023, result.getYear());
		assertEquals(Month.JUNE, result.getMonth());
		assertEquals(15, result.getDayOfMonth());
	}

	// ---- coalesce ----

	@Test
	void coalesceReturnsValueWhenNotNull() {
		DateOnly val = new DateOnly();
		DateOnly fallback = new DateOnly();
		Time.addDays(fallback, 1);
		assertEquals(val, Time.coalesce(val, fallback));
	}

	@Test
	void coalesceReturnsFallbackWhenNull() {
		DateOnly fallback = new DateOnly();
		assertEquals(fallback, Time.coalesce(null, fallback));
	}

	// ---- min ----

	@Test
	void minReturnsEarliestDate() {
		Calendar cal1 = Calendar.getInstance();
		cal1.set(2022, Calendar.JANUARY, 1);
		DateOnly d1 = new DateOnly(cal1.getTime());
		Calendar cal2 = Calendar.getInstance();
		cal2.set(2023, Calendar.JANUARY, 1);
		DateOnly d2 = new DateOnly(cal2.getTime());
		assertEquals(d1, Time.min(d1, d2));
	}

	// ---- max ----

	@Test
	void maxReturnsLatestDate() {
		Calendar cal1 = Calendar.getInstance();
		cal1.set(2022, Calendar.JANUARY, 1);
		DateOnly d1 = new DateOnly(cal1.getTime());
		Calendar cal2 = Calendar.getInstance();
		cal2.set(2023, Calendar.JANUARY, 1);
		DateOnly d2 = new DateOnly(cal2.getTime());
		assertEquals(d2, Time.max(d1, d2));
	}

	// ---- withDate ----

	@Test
	void withDateReturnsCorrectDateOnly() {
		DateOnly result = Time.withDate(20, 7, 2023);
		assertNotNull(result);
		assertEquals(2023, Time.getYear(result));
		assertEquals(7, Time.getMonthStartingFrom1(result));
		assertEquals(20, Time.getDay(result));
	}
}
