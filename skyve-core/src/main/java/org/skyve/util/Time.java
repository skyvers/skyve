package org.skyve.util;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;

import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.impl.util.TimeUtil;

/**
 * Provides date/time utility operations used across Skyve runtime and tests.
 *
 * <p>This facade delegates to {@link org.skyve.impl.util.TimeUtil} for field mutation,
 * range/difference calculations, financial-year helpers, and date arithmetic.
 * Methods mutate the provided {@link java.util.Date} instances in place unless they
 * explicitly return a derived value.
 */
public class Time {
	/**
	 * Disallow instantiation
	 */
	private Time() {
	}

	/**
	 * Clears hour, minute, second, and millisecond components on the supplied date.
	 *
	 * @param date The date instance to mutate
	 */
	public static void clearTimeComponent(Date date) {
		TimeUtil.clearTimeComponent(date);
	}

	/**
	 * Clears second and millisecond components on the supplied date/time value.
	 *
	 * @param date The date instance to mutate
	 */
	public static void clearSecondAndMillisecondComponent(Date date) {
		TimeUtil.clearSecondAndMillisecondComponent(date);
	}

	/**
	 * Clears the millisecond component of the date/time.
	 *
	 * @param date The date instance to mutate
	 */
	public static final void clearMillisecondComponent(Date date) {
		TimeUtil.clearMillisecondComponent(date);
	}

	/**
	 * Clears the date component to epoch - 1/1/1970.
	 * @param date	The date to clear
	 */
	public static void clearDateComponent(Date date) {
		TimeUtil.clearDateComponent(date);
	}
	
	/**
	 * Sets the time-of-day fields on the supplied date.
	 *
	 * @param date The date instance to mutate
	 * @param hours24 Hour of day in 24-hour format (0-23)
	 * @param minutes Minute of hour (0-59)
	 * @param seconds Second of minute (0-59)
	 */
	public static final void setTime(Date date, int hours24, int minutes, int seconds) {
		TimeUtil.setTime(date, hours24, minutes, seconds);
	}

	/**
	 * Sets the calendar date fields on the supplied date.
	 *
	 * @param date The date instance to mutate
	 * @param dayOfMonth Day of month (1-31)
	 * @param monthStartingAt1 Month of year (1-12)
	 * @param year Year value
	 */
	public static final void setDate(Date date, int dayOfMonth, int monthStartingAt1, int year) {
		TimeUtil.setDate(date, dayOfMonth, monthStartingAt1, year);
	}

	/**
	 * Returns the year component from a date.
	 *
	 * @param date Source date
	 * @return Year component
	 */
	public static final int getYear(Date date) {
		return TimeUtil.getYear(date);
	}

	/**
	 * Returns the month component as a one-based value.
	 *
	 * @param date Source date
	 * @return Month in range 1-12
	 */
	public static final int getMonthStartingFrom1(Date date) {
		return TimeUtil.getMonthStartingFrom1(date);
	}

	/**
	 * Returns the month component as a zero-based value.
	 *
	 * @param date Source date
	 * @return Month in range 0-11
	 */
	public static final int getMonthStartingFrom0(Date date) {
		return TimeUtil.getMonthStartingFrom0(date);
	}

	/**
	 * Returns the day-of-month component.
	 *
	 * @param date Source date
	 * @return Day of month in range 1-31
	 */
	public static final int getDay(Date date) {
		return TimeUtil.getDay(date);
	}

	/**
	 * Determines whether a year is a leap year.
	 *
	 * @param year Year value
	 * @return {@code true} when the year is leap, otherwise {@code false}
	 */
	public static final boolean isLeapYear(int year) {
		return TimeUtil.isLeapYear(year);
	}
	
	/**
	 * Returns the number of days in a year.
	 *
	 * @param year Year value
	 * @return 365 or 366 depending on leap-year status
	 */
	public static final int getDaysInYear(int year) {
		return TimeUtil.getDaysInYear(year);
	}
	
	/**
	 * Adds a number of hours to a date.
	 * 
	 * @param date The date instance to mutate
	 * @param numberOfHours The number of hours to add (may be negative)
	 */
	public static final void addHours(Date date, int numberOfHours) {
		TimeUtil.addHours(date, numberOfHours);
	}
	
	/**
	 * Adds a number of minutes to a date.
	 * 
	 * @param date The date to add minutes to
	 * @param numberOfMinutes The number of minutes to add (this can be negative)
	 */
	public static final void addMinutes(Date date, int numberOfMinutes) {
		TimeUtil.addMinutes(date, numberOfMinutes);
	}

	/**
	 * Adds a number of seconds to a date.
	 * 
	 * @param date The date to add seconds to
	 * @param numberOfSeconds The number of seconds to add (this can be negative)
	 */
	public static final void addSeconds(Date date, int numberOfSeconds) {
		TimeUtil.addSeconds(date, numberOfSeconds);
	}

	/**
	 * Adds a number of days to a date.
	 * 
	 * @param date	The date to add to.
	 * @param numberOfDays The number of days to add (This can be negative).
	 */
	public static final void addDays(Date date, int numberOfDays) {
		TimeUtil.addDays(date, numberOfDays);
	}

	/**
	 * Adds a number of months to a date.
	 * 
	 * @param date	The date to add months to.
	 * @param numberOfMonths The number of months to add (This can be negative).
	 */
	public static final void addMonths(Date date, int numberOfMonths) {
		TimeUtil.addMonths(date, numberOfMonths);
	}

	/**
	 * Returns the Financial Year in which this date exists.
	 * 
	 * <p>Note: this calculation assumes financial year boundaries starting on July 1.
	 *
	 * @param date Source date
	 * @return Financial year number for the supplied date
	 */
	public static final int getFinancialYear(Date date) {
		return TimeUtil.getFinancialYear(date);
	}

	/**
	 * Returns the Financial Year String in which this date exists.
	 * 
	 * <p>Note: this calculation assumes financial year boundaries starting on July 1.
	 *
	 * @param date Source date
	 * @return Financial year label (for example {@code 2025/26})
	 */
	public static final String getFinancialYearString(Date date) {
		return TimeUtil.getFinancialYearString(date);
	}

	/**
	 * Returns the number of hours between 2 times.
	 * @param start	First time in the time range
	 * @param end	Second time in the time range
	 * @return	The number of hours between startTime and endTime {negative if startTime.after(endTime)}
	 */
	public static final Decimal5 numberOfHoursBetween(TimeOnly start, TimeOnly end) {
		return TimeUtil.numberOfHoursBetween(start, end);
	}

	/**
	 * Returns the number of days between 2 dates. 
	 * If startDate.after(endDate) then the result is negative.
	 * Note: The number of days between 1/7/2002 and 2/7/2002 is 1.
	 * 
	 * @param startDate First date in date range
	 * @param endDate Second date in date range
	 * @return The number of days between startDate and endDate {negative if startDate.after(endDate)}
	 */
	public static final int numberOfDaysBetween(Date startDate, Date endDate) {
		return TimeUtil.numberOfDaysBetween(startDate, endDate);
	}

	/**
	 * Returns the number of days in a range defined by 2 dates. 
	 * If startDate.after(endDate) then the result is negative.
	 * Note: The number of days in the range 1/7/2002 and 2/7/2002 is 2.
	 * 
	 * @param startDate First date in date range
	 * @param endDate Second date in date range
	 * @return The number of days in the range between startDate and endDate (negative if startDate.after(endDate).
	 */
	public static final int numberOfDaysInRange(Date startDate, Date endDate) {
		return TimeUtil.numberOfDaysInRange(startDate, endDate);
	}

	/**
	 * Rolls a date forward to the next occurrence of a specified day-of-week.
	 * 
	 * @param date The date instance to mutate
	 * @param dayOfWeek Day-of-week constant from {@link Calendar} (for example {@link Calendar#MONDAY})
	 * @return The same mutated date instance for chaining
	 */
	public static final Date findNextDayOfWeek(Date date, int dayOfWeek) {
		return TimeUtil.findNextDayOfWeek(date, dayOfWeek);
	}
	
	/**
	 * Ensures the supplied date falls on a work day.
	 * 
	 * <p>If the date is on a weekend, it is advanced to the next work day.
	 *
	 * @param date The date instance to mutate
	 * @return The same mutated date instance for chaining
	 */
	public static final Date ensureWorkDay(Date date) {
		return TimeUtil.ensureWorkDay(date);
	}
	
	/**
	 * Produces a human-readable relative-day description between two dates.
	 *
	 * @param now The reference date
	 * @param agoOrUntil The comparison date
	 * @return Relative description such as "Today", "Yesterday", "Tomorrow", or day offsets
	 */
	public static final String daysBetweenDescription(Date now, Date agoOrUntil) {
		return TimeUtil.daysBetweenDescription(now, agoOrUntil);
	}
	
	/**
	 * Returns a date with a number of days added to it.
	 * 
	 * @param date The date to add to.
	 * @param numberOfDays The number of days to add (This can be negative).
	 */
	public static DateOnly addDaysToNew(final Date date, final int numberOfDays) {
		return TimeUtil.addDaysToNew(date, numberOfDays);
	}

	/**
	 * Returns a date with a number of months added to it.
	 * 
	 * @param date The date to add to.
	 * @param numberOfMonths The number of months to add (This can be negative).
	 */
	public static DateOnly addMonthsToNew(final Date date, final int numberOfMonths) {
		return TimeUtil.addMonthsToNew(date, numberOfMonths);
	}

	/**
	 * Returns a date with a number of years added to it.
	 * 
	 * @param date The date to add to.
	 * @param numberOfYears The number of years to add (This can be negative).
	 */
	public static DateOnly addYearsToNew(final Date date, final int numberOfYears) {
		return TimeUtil.addYearsToNew(date, numberOfYears);
	}

	/**
	 * Converts a {@link LocalDate} to a {@link DateOnly}.
	 * 
	 * @param localDate The local date to convert
	 * @return A DateOnly from the local date, based on the System time zone
	 */
	public static DateOnly asDateOnly(LocalDate localDate) {
		return TimeUtil.asDateOnly(localDate);
	}

	/**
	 * Converts a {@link Date} or Skyve Date to a {@link LocalDate}.
	 * 
	 * @param date The date to convert
	 * @return A LocalDate from the date, based on the System time zone
	 */
	public static LocalDate asLocalDate(final Date date) {
		return TimeUtil.asLocalDate(date);
	}

	/**
	 * Returns the first non-null date value
	 *
	 * @param val Primary value
	 * @param ifNullValue Fallback value when {@code val} is null
	 * @return {@code val} when non-null, otherwise {@code ifNullValue}
	 */
	public static DateOnly coalesce(DateOnly val, DateOnly ifNullValue) {
		return TimeUtil.coalesce(val, ifNullValue);
	}

	/**
	 * Returns the earliest date from the supplied dates
	 * 
	 * @param dates Variable list of dates to compare
	 * @return The earliest date
	 */
	public static DateOnly min(final DateOnly... dates) {
		return TimeUtil.min(dates);
	}

	/**
	 * Returns the latest date from the supplied dates
	 * 
	 * @param dates Variable list of dates to compare
	 * @return The latest date
	 */
	public static DateOnly max(final DateOnly... dates) {
		return TimeUtil.max(dates);
	}

	/**
	 * Returns a new {@link DateOnly} with the specified day, month and year.
	 * 
	 * @param dayOfMonth The day of the month (1-31)
	 * @param monthStartingAt1 The month of year (1-12)
	 * @param year The year
	 * @return A new date with the day, month and year
	 */
	public static DateOnly withDate(int dayOfMonth, int monthStartingAt1, int year) {
		return TimeUtil.withDate(dayOfMonth, monthStartingAt1, year);
	}
}
