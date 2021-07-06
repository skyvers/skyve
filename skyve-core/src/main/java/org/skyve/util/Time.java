package org.skyve.util;

import java.time.LocalDate;
import java.util.Calendar;
import java.util.Date;

import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.impl.util.TimeUtil;

/**
 * 
 */
public class Time {
	/**
	 * Disallow instantiation
	 */
	private Time() {
	}

	/**
	 * Sets the time component of the date to 12 midnight.
	 */
	public static void clearTimeComponent(Date date) {
		TimeUtil.clearTimeComponent(date);
	}
	
	/**
	 * Clears the millisecond component of the date/time.
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
	 * 
	 * @param date
	 * @param hours24
	 * @param minutes
	 * @param seconds
	 */
	public static final void setTime(Date date, int hours24, int minutes, int seconds) {
		TimeUtil.setTime(date, hours24, minutes, seconds);
	}

	/**
	 * 
	 * @param date
	 * @param dayOfMonth
	 * @param monthStartingAt1
	 * @param year
	 */
	public static final void setDate(Date date, int dayOfMonth, int monthStartingAt1, int year) {
		TimeUtil.setDate(date, dayOfMonth, monthStartingAt1, year);
	}

	/**
	 * 
	 * @param date
	 * @return
	 */
	public static final int getYear(Date date) {
		return TimeUtil.getYear(date);
	}

	/**
	 * 
	 * @param date
	 * @return
	 */
	public static final int getMonthStartingFrom1(Date date) {
		return TimeUtil.getMonthStartingFrom1(date);
	}

	/**
	 * 
	 * @param date
	 * @return
	 */
	public static final int getMonthStartingFrom0(Date date) {
		return TimeUtil.getMonthStartingFrom0(date);
	}

	/**
	 * 
	 * @param date
	 * @return
	 */
	public static final int getDay(Date date) {
		return TimeUtil.getDay(date);
	}

	/**
	 * 
	 * @param year
	 * @return
	 */
	public static final boolean isLeapYear(int year) {
		return TimeUtil.isLeapYear(year);
	}
	
	/**
	 * 
	 * @param year
	 * @return
	 */
	public static final int getDaysInYear(int year) {
		return TimeUtil.getDaysInYear(year);
	}
	
	/**
	 * Adds a number of hours to a date.
	 * 
	 * @param date	The date to add to.
	 * @param numberOfDays The number of days to add (This can be negative).
	 */
	public static final void addHours(Date date, int numberOfHours) {
		TimeUtil.addHours(date, numberOfHours);
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
	 * Note: this only applies for locales where the financial year begins on July 1.
	 */
	public static final int getFinancialYear(Date date) {
		return TimeUtil.getFinancialYear(date);
	}

	/**
	 * Returns the Financial Year String in which this date exists.
	 * 
	 * Note: this only applies for locales where the financial year begins on July 1.
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
	 * Find the next day of the week.
	 * 
	 * @param date The existing date to roll.
	 * @param dayOfWeek The int representing the day of the week - from {@link Calendar}. eg Calendar.MONDAY
	 * @return	The date passed in to allow method chaining.
	 */
	public static final Date findNextDayOfWeek(Date date, int dayOfWeek) {
		return TimeUtil.findNextDayOfWeek(date, dayOfWeek);
	}
	
	/**
	 * Find the next day of the week.
	 * 
	 * @param date The existing date to roll.
	 * @return	The date passed in to allow method chaining.
	 */
	public static final Date ensureWorkDay(Date date) {
		return TimeUtil.ensureWorkDay(date);
	}
	
	/**
	 * Output a description of the days between 2 dates
	 * @param now	The reference date to compare.
	 * @param agoOrUntil	When to compare the now date to.
	 * @return	"Today", "Yesterday", "Tomorrow" or "? days ago" or "? days time".
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
	 * Returns the first non-null date value
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
