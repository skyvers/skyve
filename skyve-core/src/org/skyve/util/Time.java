package org.skyve.util;

import java.util.Calendar;
import java.util.Date;

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
	 * Returns a date with a number of days added to it.
	 * 
	 * @param date	The date to add to.
	 * @param numberOfDays The number of days to add (This can be negative).
	 * @return	The date passed in to allow method chaining.
	 */
	public static final Date addDays(Date date, int numberOfDays) {
		return TimeUtil.addDays(date, numberOfDays);
	}

	/**
	 * Returns a date with a number of months added to it.
	 * 
	 * @param date	The date to add months to.
	 * @param numberOfMonths The number of months to add (This can be negative).
	 * @return	The date passed in to allow method chaining.
	 */
	public static final Date addMonths(Date date, int numberOfMonths) {
		return TimeUtil.addMonths(date, numberOfMonths);
	}

	/**
	 * Returns the Financial Year in which this date exists.
	 */
	public static final int getFinancialYear(Date date) {
		return TimeUtil.getFinancialYear(date);
	}

	/**
	 * Returns the Financial Year String in which this date exists.
	 */
	public static final String getFinancialYearString(Date date) {
		return TimeUtil.getFinancialYearString(date);
	}

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
	 * @return The number of days between startDate and endDate (negative if startDate.after(endDate).
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
}
