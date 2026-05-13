package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

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
		assertThat(result.get(Calendar.HOUR_OF_DAY), is(0));
		assertThat(result.get(Calendar.MINUTE), is(0));
		assertThat(result.get(Calendar.SECOND), is(0));
		assertThat(result.get(Calendar.MILLISECOND), is(0));
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
		assertThat(result.get(Calendar.MILLISECOND), is(0));
		assertThat(result.get(Calendar.HOUR_OF_DAY), is(10));
		assertThat(result.get(Calendar.MINUTE), is(30));
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
		assertThat(result.get(Calendar.SECOND), is(0));
		assertThat(result.get(Calendar.MILLISECOND), is(0));
		assertThat(result.get(Calendar.MINUTE), is(30));
		assertThat(result.get(Calendar.HOUR_OF_DAY), is(10));
	}

	@Test
	void clearDateComponentSetsEpochDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 30, 0);
		Date date = cal.getTime();

		TimeUtil.clearDateComponent(date);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.YEAR), is(1970));
		assertThat(result.get(Calendar.MONTH), is(Calendar.JANUARY));
		assertThat(result.get(Calendar.DAY_OF_MONTH), is(1));
	}

	@Test
	void setTimeSetsCorrectHoursMinutesSeconds() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.setTime(date, 14, 25, 30);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.HOUR_OF_DAY), is(14));
		assertThat(result.get(Calendar.MINUTE), is(25));
		assertThat(result.get(Calendar.SECOND), is(30));
		assertThat(result.get(Calendar.MILLISECOND), is(0));
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
		assertThat(result.get(Calendar.HOUR_OF_DAY), is(14));
		assertThat(result.get(Calendar.MINUTE), is(30));
	}

	@Test
	void setDateSetsCorrectDayMonthYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2000, Calendar.JANUARY, 1);
		Date date = cal.getTime();

		TimeUtil.setDate(date, 20, 7, 2023);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.YEAR), is(2023));
		assertThat(result.get(Calendar.MONTH), is(Calendar.JULY));
		assertThat(result.get(Calendar.DAY_OF_MONTH), is(20));
	}

	@Test
	void getYearReturnsCorrectYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.YEAR, 2023);
		assertThat(TimeUtil.getYear(cal.getTime()), is(2023));
	}

	@Test
	void getMonthStartingFrom1ReturnsCorrectMonth() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.MONTH, Calendar.MARCH);
		assertThat(TimeUtil.getMonthStartingFrom1(cal.getTime()), is(3));
	}

	@Test
	void getMonthStartingFrom0ReturnsCorrectMonth() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.MONTH, Calendar.MARCH);
		assertThat(TimeUtil.getMonthStartingFrom0(cal.getTime()), is(Calendar.MARCH));
	}

	@Test
	void getDayReturnsCorrectDay() {
		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.DAY_OF_MONTH, 15);
		assertThat(TimeUtil.getDay(cal.getTime()), is(15));
	}

	@Test
	void isLeapYearTrueFor2000() {
		assertThat(TimeUtil.isLeapYear(2000), is(true));
	}

	@Test
	void isLeapYearTrueFor2024() {
		assertThat(TimeUtil.isLeapYear(2024), is(true));
	}

	@Test
	void isLeapYearFalseFor2023() {
		assertThat(TimeUtil.isLeapYear(2023), is(false));
	}

	@Test
	void isLeapYearFalseFor1900() {
		assertThat(TimeUtil.isLeapYear(1900), is(false));
	}

	@Test
	void getDaysInYearReturns366ForLeapYear() {
		assertThat(TimeUtil.getDaysInYear(2024), is(366));
	}

	@Test
	void getDaysInYearReturns365ForNonLeapYear() {
		assertThat(TimeUtil.getDaysInYear(2023), is(365));
	}

	@Test
	void addHoursIncreasesTime() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addHours(date, 3);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.HOUR_OF_DAY), is(13));
	}

	@Test
	void addMinutesIncreasesTime() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addMinutes(date, 30);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.MINUTE), is(30));
	}

	@Test
	void addSecondsIncreasesTime() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 10, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addSeconds(date, 45);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.SECOND), is(45));
	}

	@Test
	void addDaysIncreasesDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addDays(date, 5);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.DAY_OF_MONTH), is(20));
	}

	@Test
	void addDaysNegativeDecreasesDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addDays(date, -5);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.DAY_OF_MONTH), is(10));
	}

	@Test
	void addMonthsIncreasesDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JANUARY, 15, 0, 0, 0);
		Date date = cal.getTime();

		TimeUtil.addMonths(date, 3);

		Calendar result = Calendar.getInstance();
		result.setTime(date);
		assertThat(result.get(Calendar.MONTH), is(Calendar.APRIL));
	}

	@Test
	void getFinancialYearForJulyReturnsCurrentYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JULY, 1);
		assertThat(TimeUtil.getFinancialYear(cal.getTime()), is(2023));
	}

	@Test
	void getFinancialYearForJuneReturnsPreviousYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 30);
		assertThat(TimeUtil.getFinancialYear(cal.getTime()), is(2022));
	}

	@Test
	void getFinancialYearForJanuaryReturnsPreviousYear() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JANUARY, 1);
		assertThat(TimeUtil.getFinancialYear(cal.getTime()), is(2022));
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
		assertThat(result.compareTo(new Decimal5(2.5)) == 0, is(true));
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
		assertThat(result.compareTo(new Decimal5(1.0)) == 0, is(true));
	}

	@Test
	void numberOfDaysBetweenZeroForSameDay() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		Date d = cal.getTime();
		assertThat(TimeUtil.numberOfDaysBetween(d, d), is(0));
	}

	@Test
	void numberOfDaysBetweenPositiveForFutureDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 10);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 15);
		Date end = cal.getTime();
		assertThat(TimeUtil.numberOfDaysBetween(start, end), is(5));
	}

	@Test
	void numberOfDaysBetweenNegativeForPastDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 10);
		Date end = cal.getTime();
		assertThat(TimeUtil.numberOfDaysBetween(start, end), is(-5));
	}

	@Test
	void numberOfDaysInRangeIncludesBothEndpoints() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 10);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 15);
		Date end = cal.getTime();
		assertThat(TimeUtil.numberOfDaysInRange(start, end), is(6));
	}

	@Test
	void numberOfDaysInRangeNegativeDecrements() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		Date start = cal.getTime();
		cal.set(2023, Calendar.JUNE, 10);
		Date end = cal.getTime();
		assertThat(TimeUtil.numberOfDaysInRange(start, end), is(-6));
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
		assertThat(result.endsWith(" days ago"), is(true));
	}

	@Test
	void daysBetweenDescriptionDaysTimeForFuture() {
		Calendar now = Calendar.getInstance();
		Calendar future = Calendar.getInstance();
		future.add(Calendar.DAY_OF_MONTH, 5);
		String result = TimeUtil.daysBetweenDescription(now.getTime(), future.getTime());
		assertThat(result.endsWith(" days time"), is(true));
	}

	@Test
	void addDaysToNewReturnsNewDateOnly() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 10);
		DateOnly result = TimeUtil.addDaysToNew(cal.getTime(), 5);
		assertThat(result, notNullValue());
		Calendar resultCal = Calendar.getInstance();
		resultCal.setTime(result);
		assertThat(resultCal.get(Calendar.DAY_OF_MONTH), is(15));
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
		assertThat(resultCal.get(Calendar.MONTH), is(Calendar.MARCH));
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
		assertThat(resultCal.get(Calendar.YEAR), is(2023));
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
		assertThat(TimeUtil.getYear(result), is(2023));
		assertThat(TimeUtil.getMonthStartingFrom1(result), is(6));
		assertThat(TimeUtil.getDay(result), is(15));
	}

	@Test
	void asLocalDateConvertsDate() {
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 15);
		LocalDate result = TimeUtil.asLocalDate(cal.getTime());
		assertThat(result.getYear(), is(2023));
		assertThat(result.getMonthValue(), is(6));
		assertThat(result.getDayOfMonth(), is(15));
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
		assertThrows(IllegalArgumentException.class, () -> TimeUtil.min());
	}

	@Test
	void maxThrowsForEmptyArray() {
		assertThrows(IllegalArgumentException.class, () -> TimeUtil.max());
	}

	@Test
	void withDateReturnsDateWithCorrectComponents() {
		DateOnly result = TimeUtil.withDate(20, 7, 2023);
		assertThat(TimeUtil.getYear(result), is(2023));
		assertThat(TimeUtil.getMonthStartingFrom1(result), is(7));
		assertThat(TimeUtil.getDay(result), is(20));
	}

	@Test
	void parseISODateParsesBasicFormat() throws Exception {
		Date result = TimeUtil.parseISODate("2023-06-15T10:30:00.000+10:00");
		assertThat(result, notNullValue());
	}

	@Test
	void parseISODateParsesZuluFormat() throws Exception {
		Date result = TimeUtil.parseISODate("2023-06-15T10:30:00.000Z");
		assertThat(result, notNullValue());
	}

	@Test
	void parseISODateParsesNoMillisNoTz() throws Exception {
		Date result = TimeUtil.parseISODate("2023-06-15T10:30:00");
		assertThat(result, notNullValue());
	}

	@Test
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
		assertThat(result.contains("2023"), is(true));
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
		assertThat(result.get(Calendar.DAY_OF_WEEK), is(Calendar.FRIDAY));
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
		assertThat(result.get(Calendar.DAY_OF_WEEK), is(Calendar.MONDAY));
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
		assertThat(result.get(Calendar.DAY_OF_WEEK), is(Calendar.MONDAY));
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
		assertThat(result.get(Calendar.DAY_OF_WEEK), is(Calendar.MONDAY));
	}

	@Test
	void ensureWorkDayMondayUnchanged() {
		// June 12, 2023 is a Monday
		Calendar cal = Calendar.getInstance();
		cal.set(2023, Calendar.JUNE, 12);
		Date date = cal.getTime();
		long before = date.getTime();

		TimeUtil.ensureWorkDay(date);

		assertThat(date.getTime(), is(before));
	}
}
