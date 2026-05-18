package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;

@SuppressWarnings("static-method")
class TimeUtilTest {

	@Test
	void clearTimeComponentZerosHoursMinutesSecondsMillis() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 45);
		cal.set(Calendar.MILLISECOND, 500);
		Date date = cal.getTime();

		TimeUtil.clearTimeComponent(date);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(0, result.get(Calendar.HOUR_OF_DAY));
		assertEquals(0, result.get(Calendar.MINUTE));
		assertEquals(0, result.get(Calendar.SECOND));
		assertEquals(0, result.get(Calendar.MILLISECOND));
	}

	@Test
	void clearMillisecondComponentZerosMillis() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 45);
		cal.set(Calendar.MILLISECOND, 999);
		Date date = cal.getTime();

		TimeUtil.clearMillisecondComponent(date);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(0, result.get(Calendar.MILLISECOND));
		assertEquals(10, result.get(Calendar.HOUR_OF_DAY));
		assertEquals(30, result.get(Calendar.MINUTE));
	}

	@Test
	void clearSecondAndMillisecondComponentZerosBoth() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 45);
		cal.set(Calendar.MILLISECOND, 500);
		Date date = cal.getTime();

		TimeUtil.clearSecondAndMillisecondComponent(date);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(0, result.get(Calendar.SECOND));
		assertEquals(0, result.get(Calendar.MILLISECOND));
		assertEquals(30, result.get(Calendar.MINUTE));
		assertEquals(10, result.get(Calendar.HOUR_OF_DAY));
	}

	@Test
	void clearDateComponentSetsEpochDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 0);
		Date date = cal.getTime();

		TimeUtil.clearDateComponent(date);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(1970, result.get(Calendar.YEAR));
		assertEquals(Calendar.JANUARY, result.get(Calendar.MONTH));
		assertEquals(1, result.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	void setTimeSetsCorrectHoursMinutesSeconds() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.setTime(date, 14, 25, 30);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(14, result.get(Calendar.HOUR_OF_DAY));
		assertEquals(25, result.get(Calendar.MINUTE));
		assertEquals(30, result.get(Calendar.SECOND));
		assertEquals(0, result.get(Calendar.MILLISECOND));
	}

	@Test
	void setTimeWithTimeOnlySetsCorrectTime() {
		Calendar timeCal = Calendar.getInstance();
		timeCal.set(1970, Calendar.JANUARY, 1, 14, 30, 0);
		timeCal.set(Calendar.MILLISECOND, 0);
		TimeOnly timeOnly = new TimeOnly(timeCal.getTimeInMillis());

		Calendar dateCal = Calendar.getInstance();
		dateCal.set(2023, Calendar.JUNE, 15, 0, 0, 0);
		Date date = dateCal.getTime();

		TimeUtil.setTime(date, timeOnly);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(14, result.get(Calendar.HOUR_OF_DAY));
		assertEquals(30, result.get(Calendar.MINUTE));
	}

	@Test
	void setDateSetsCorrectDayMonthYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2000, Calendar.JANUARY, 1);
		Date date = cal.getTime();

		TimeUtil.setDate(date, 20, 7, 2023);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(2023, result.get(Calendar.YEAR));
		assertEquals(Calendar.JULY, result.get(Calendar.MONTH));
		assertEquals(20, result.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	void getYearReturnsCorrectYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.YEAR, 2023);
		assertEquals(2023, TimeUtil.getYear(cal.getTime()));
	}

	@Test
	void getMonthStartingFrom1ReturnsCorrectMonth() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.MONTH, Calendar.MARCH);
		assertEquals(3, TimeUtil.getMonthStartingFrom1(cal.getTime()));
	}

	@Test
	void getMonthStartingFrom0ReturnsCorrectMonth() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.MONTH, Calendar.MARCH);
		assertEquals(Calendar.MARCH, TimeUtil.getMonthStartingFrom0(cal.getTime()));
	}

	@Test
	void getDayReturnsCorrectDay() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.DAY_OF_MONTH, 15);
		assertEquals(15, TimeUtil.getDay(cal.getTime()));
	}

	@Test
	void isLeapYearTrueFor2000() {
		assertTrue(TimeUtil.isLeapYear(2000));
	}

	@Test
	void isLeapYearTrueFor2024() {
		assertTrue(TimeUtil.isLeapYear(2024));
	}

	@Test
	void isLeapYearFalseFor2023() {
		assertFalse(TimeUtil.isLeapYear(2023));
	}

	@Test
	void isLeapYearFalseFor1900() {
		assertFalse(TimeUtil.isLeapYear(1900));
	}

	@Test
	void getDaysInYearReturns366ForLeapYear() {
		assertEquals(366, TimeUtil.getDaysInYear(2024));
	}

	@Test
	void getDaysInYearReturns365ForNonLeapYear() {
		assertEquals(365, TimeUtil.getDaysInYear(2023));
	}

	@Test
	void addHoursIncreasesTime() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addHours(date, 3);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(13, result.get(Calendar.HOUR_OF_DAY));
	}

	@Test
	void addMinutesIncreasesTime() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addMinutes(date, 30);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(30, result.get(Calendar.MINUTE));
	}

	@Test
	void addSecondsIncreasesTime() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addSeconds(date, 45);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(45, result.get(Calendar.SECOND));
	}

	@Test
	void addDaysIncreasesDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addDays(date, 5);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(20, result.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	void addDaysNegativeDecreasesDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addDays(date, -5);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(10, result.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	void addMonthsIncreasesDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JANUARY, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addMonths(date, 3);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(Calendar.APRIL, result.get(Calendar.MONTH));
	}

	@Test
	void getFinancialYearForJulyReturnsCurrentYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JULY, 1);
		assertEquals(2023, TimeUtil.getFinancialYear(cal.getTime()));
	}

	@Test
	void getFinancialYearForJuneReturnsPreviousYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 30);
		assertEquals(2022, TimeUtil.getFinancialYear(cal.getTime()));
	}

	@Test
	void getFinancialYearForJanuaryReturnsPreviousYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JANUARY, 1);
		assertEquals(2022, TimeUtil.getFinancialYear(cal.getTime()));
	}

	@Test
	void getFinancialYearStringReturnsExpectedFormat() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JULY, 1);
		assertThat(TimeUtil.getFinancialYearString(cal.getTime()), is("2023/2024"));
	}

	@Test
	void numberOfHoursBetweenReturnsHalfHours() {
		Calendar cal = Calendar.getInstance();
		cal.set(1970, Calendar.JANUARY, 1, 8, 0, 0);
		cal.set(Calendar.MILLISECOND, 0);
		TimeOnly start = new TimeOnly(cal.getTimeInMillis());
		cal.set(1970, Calendar.JANUARY, 1, 10, 30, 0);
		TimeOnly end = new TimeOnly(cal.getTimeInMillis());

		Decimal5 result = TimeUtil.numberOfHoursBetween(start, end);
		assertEquals(0, result.compareTo(new Decimal5(2.5)));
	}

	@Test
	void numberOfHoursBetweenOneHour() {
		Calendar cal = Calendar.getInstance();
		cal.set(1970, Calendar.JANUARY, 1, 9, 0, 0);
		cal.set(Calendar.MILLISECOND, 0);
		TimeOnly start = new TimeOnly(cal.getTimeInMillis());
		cal.set(1970, Calendar.JANUARY, 1, 10, 0, 0);
		TimeOnly end = new TimeOnly(cal.getTimeInMillis());

		Decimal5 result = TimeUtil.numberOfHoursBetween(start, end);
		assertEquals(0, result.compareTo(new Decimal5(1.0)));
	}

	@Test
	void numberOfDaysBetweenZeroForSameDay() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		Date d = cal.getTime();
		assertEquals(0, TimeUtil.numberOfDaysBetween(d, d));
	}

	@Test
	void numberOfDaysBetweenPositiveForFutureDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 10);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 15);
		Date end = cal.getTime();
		assertEquals(5, TimeUtil.numberOfDaysBetween(start, end));
	}

	@Test
	void numberOfDaysBetweenNegativeForPastDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 10);
		Date end = cal.getTime();
		assertEquals(-5, TimeUtil.numberOfDaysBetween(start, end));
	}

	@Test
	void numberOfDaysInRangeIncludesBothEndpoints() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 10);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 15);
		Date end = cal.getTime();
		assertEquals(6, TimeUtil.numberOfDaysInRange(start, end));
	}

	@Test
	void numberOfDaysInRangeNegativeDecrements() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 10);
		Date end = cal.getTime();
		assertEquals(-6, TimeUtil.numberOfDaysInRange(start, end));
	}

	@Test
	void daysBetweenDescriptionTodayForSameDay() {
		Calendar cal = Calendar.getInstance();
		Date now = cal.getTime();
		assertThat(TimeUtil.daysBetweenDescription(now, now), is("Today"));
	}

	@Test
	void daysBetweenDescriptionTomorrowForNextDay() {
		Calendar now = Calendar.getInstance();
		Calendar tomorrow = Calendar.getInstance();
		tomorrow.add(Calendar.DAY_OF_MONTH, 1);
		assertThat(TimeUtil.daysBetweenDescription(now.getTime(), tomorrow.getTime()), is("Tomorrow"));
	}

	@Test
	void daysBetweenDescriptionYesterdayForPreviousDay() {
		Calendar now = Calendar.getInstance();
		Calendar yesterday = Calendar.getInstance();
		yesterday.add(Calendar.DAY_OF_MONTH, -1);
		assertThat(TimeUtil.daysBetweenDescription(now.getTime(), yesterday.getTime()), is("Yesterday"));
	}

	@Test
	void daysBetweenDescriptionDaysAgoForPast() {
		Calendar now = Calendar.getInstance();
		Calendar past = Calendar.getInstance();
		past.add(Calendar.DAY_OF_MONTH, -5);
		String result = TimeUtil.daysBetweenDescription(now.getTime(), past.getTime());
		assertTrue(result.endsWith(" days ago"));
	}

	@Test
	void daysBetweenDescriptionDaysTimeForFuture() {
		Calendar now = Calendar.getInstance();
		Calendar future = Calendar.getInstance();
		future.add(Calendar.DAY_OF_MONTH, 5);
		String result = TimeUtil.daysBetweenDescription(now.getTime(), future.getTime());
		assertTrue(result.endsWith(" days time"));
	}

	@Test
	void addDaysToNewReturnsNewDateOnly() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 10);
		DateOnly result = TimeUtil.addDaysToNew(cal.getTime(), 5);
		assertThat(result, notNullValue());
		Calendar resultCal = Calendar.getInstance();
		resultCal.setTime(result);
		assertEquals(15, resultCal.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	void addDaysToNewThrowsForNullDate() {
		assertThrows(IllegalArgumentException.class, () -> TimeUtil.addDaysToNew(null, 5));
	}

	@Test
	void addMonthsToNewReturnsNewDateOnly() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JANUARY, 15);
		DateOnly result = TimeUtil.addMonthsToNew(cal.getTime(), 2);
		assertThat(result, notNullValue());
		Calendar resultCal = Calendar.getInstance();
		resultCal.setTime(result);
		assertEquals(Calendar.MARCH, resultCal.get(Calendar.MONTH));
	}

	@Test
	void addMonthsToNewThrowsForNullDate() {
		assertThrows(IllegalArgumentException.class, () -> TimeUtil.addMonthsToNew(null, 2));
	}

	@Test
	void addYearsToNewReturnsNewDateOnly() {
		Calendar cal = Calendar.getInstance();
		cal.set(2020, Calendar.JUNE, 15);
		DateOnly result = TimeUtil.addYearsToNew(cal.getTime(), 3);
		assertThat(result, notNullValue());
		Calendar resultCal = Calendar.getInstance();
		resultCal.setTime(result);
		assertEquals(2023, resultCal.get(Calendar.YEAR));
	}

	@Test
	void addYearsToNewThrowsForNullDate() {
		assertThrows(IllegalArgumentException.class, () -> TimeUtil.addYearsToNew(null, 1));
	}

	@Test
	void asDateOnlyConvertsLocalDate() {
		LocalDate localDate = LocalDate.of(2023, 6, 15);
		DateOnly result = TimeUtil.asDateOnly(localDate);
		assertThat(result, notNullValue());
		assertEquals(2023, TimeUtil.getYear(result));
		assertEquals(6, TimeUtil.getMonthStartingFrom1(result));
		assertEquals(15, TimeUtil.getDay(result));
	}

	@Test
	void asLocalDateConvertsDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		LocalDate result = TimeUtil.asLocalDate(cal.getTime());
		assertEquals(2023, result.getYear());
		assertEquals(6, result.getMonthValue());
		assertEquals(15, result.getDayOfMonth());
	}

	@Test
	void coalesceReturnsValueWhenNotNull() {
		DateOnly val = new DateOnly();
		DateOnly fallback = new DateOnly();
		assertThat(TimeUtil.coalesce(val, fallback), is(val));
	}

	@Test
	void coalesceReturnsFallbackWhenNull() {
		DateOnly fallback = new DateOnly();
		assertThat(TimeUtil.coalesce(null, fallback), is(fallback));
	}

	@Test
	void minReturnsSmallestDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JANUARY, 10);
		DateOnly d1 = new DateOnly(cal.getTime());
		cal.set(2023, Calendar.JUNE, 15);
		DateOnly d2 = new DateOnly(cal.getTime());
		assertThat(TimeUtil.min(d1, d2), is(d1));
	}

	@Test
	void maxReturnsLargestDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JANUARY, 10);
		DateOnly d1 = new DateOnly(cal.getTime());
		cal.set(2023, Calendar.JUNE, 15);
		DateOnly d2 = new DateOnly(cal.getTime());
		assertThat(TimeUtil.max(d1, d2), is(d2));
	}

	@Test
	void minThrowsForEmptyArray() {
		assertThrows(IllegalArgumentException.class, TimeUtil::min);
	}

	@Test
	void maxThrowsForEmptyArray() {
		assertThrows(IllegalArgumentException.class, TimeUtil::max);
	}

	@Test
	void withDateReturnsDateWithCorrectComponents() {
		DateOnly result = TimeUtil.withDate(20, 7, 2023);
		assertEquals(2023, TimeUtil.getYear(result));
		assertEquals(7, TimeUtil.getMonthStartingFrom1(result));
		assertEquals(20, TimeUtil.getDay(result));
	}

	@Test
	@SuppressWarnings("java:S5976")
	void parseISODateParsesBasicFormat() throws Exception {
		Date result = TimeUtil.parseISODate("2023-06-15T10:30:00.000+10:00");
		assertThat(result, notNullValue());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void parseISODateParsesZuluFormat() throws Exception {
		Date result = TimeUtil.parseISODate("2023-06-15T10:30:00.000Z");
		assertThat(result, notNullValue());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void parseISODateParsesNoMillisNoTz() throws Exception {
		Date result = TimeUtil.parseISODate("2023-06-15T10:30:00");
		assertThat(result, notNullValue());
	}

	@Test
	@SuppressWarnings("java:S5976")
	void parseISODateParsesNoMillisZuluFormat() throws Exception {
		Date result = TimeUtil.parseISODate("2023-06-15T10:30:00Z");
		assertThat(result, notNullValue());
	}

	@Test
	void formatISODateReturnsNonNullString() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 0);
		String result = TimeUtil.formatISODate(cal.getTime(), false);
		assertThat(result, notNullValue());
		assertTrue(result.contains("2023"));
	}

	@Test
	void formatISODateInUTCReturnsNonNullString() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 0);
		String result = TimeUtil.formatISODate(cal.getTime(), true);
		assertThat(result, notNullValue());
	}

	@Test
	void findNextDayOfWeekFromMondayToFriday() {
		// June 12, 2023 is a Monday
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 12);
		Date date = cal.getTime();

		TimeUtil.findNextDayOfWeek(date, Calendar.FRIDAY);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(Calendar.FRIDAY, result.get(Calendar.DAY_OF_WEEK));
	}

	@Test
	void findNextDayOfWeekSameDayReturnsUnchanged() {
		// If already on the target day, returns that day
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 12); // Monday
		Date date = cal.getTime();

		TimeUtil.findNextDayOfWeek(date, Calendar.MONDAY);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(Calendar.MONDAY, result.get(Calendar.DAY_OF_WEEK));
	}

	@Test
	void ensureWorkDaySaturdayBecomesMonday() {
		// June 10, 2023 is a Saturday
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 10);
		Date date = cal.getTime();

		TimeUtil.ensureWorkDay(date);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(Calendar.MONDAY, result.get(Calendar.DAY_OF_WEEK));
	}

	@Test
	void ensureWorkDaySundayBecomesMonday() {
		// June 11, 2023 is a Sunday
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 11);
		Date date = cal.getTime();

		TimeUtil.ensureWorkDay(date);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertEquals(Calendar.MONDAY, result.get(Calendar.DAY_OF_WEEK));
	}

	@Test
	void ensureWorkDayMondayUnchanged() {
		// June 12, 2023 is a Monday
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 12);
		Date date = cal.getTime();
		long before = date.getTime();

		TimeUtil.ensureWorkDay(date);

		assertEquals(before, date.getTime());
	}
}
