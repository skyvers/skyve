package org.skyve.impl.util;

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;

public class TimeUtil {
	private TimeUtil() {
		// no implementation
	}

	/**
	 * Sets the time component of the calendar to 12 midnight.
	 */
	public static final void clearTimeComponent(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * Clears the millisecond component of the calendar.
	 */
	public static final void clearMillisecondComponent(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		date.setTime(calendar.getTime().getTime());
	} // clearTimeComponent

	/**
	 * Clears the date component to epoch - 1/1/1970.
	 * @param date	The date to clear
	 */
	public static void clearDateComponent(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.DAY_OF_MONTH, 1);
		calendar.set(Calendar.MONTH, 0);
		calendar.set(Calendar.YEAR, 1970);
		date.setTime(calendar.getTime().getTime());
	}
	
	public static final void setTime(Date date, int hours24, int minutes, int seconds) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, seconds);
		calendar.set(Calendar.MINUTE, minutes);
		calendar.set(Calendar.HOUR_OF_DAY, hours24);
		date.setTime(calendar.getTime().getTime());
	}

	public static final void setTime(Date date, TimeOnly time) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(time);
		int hours = calendar.get(Calendar.HOUR_OF_DAY);
		int minutes= calendar.get(Calendar.MINUTE);
		int seconds=  calendar.get(Calendar.SECOND);
		//int millis = calendar.get(Calendar.MILLISECOND);
		
		setTime(date, hours, minutes, seconds);
	}
	
	public static final void setDate(Date date, int dayOfMonth, int monthStartingAt1, int year) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		calendar.set(Calendar.DAY_OF_MONTH, dayOfMonth);
		calendar.set(Calendar.MONTH, monthStartingAt1 - 1);
		calendar.set(Calendar.YEAR, year);
		date.setTime(calendar.getTime().getTime());
	}

	public static final int getYear(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);

		return calendar.get(Calendar.YEAR);
	}

	public static final int getMonthStartingFrom1(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);

		return calendar.get(Calendar.MONTH) + 1;
	}

	public static final int getMonthStartingFrom0(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);

		return calendar.get(Calendar.MONTH);
	}

	public static final int getDay(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);

		return calendar.get(Calendar.DAY_OF_MONTH);
	}

	public static final boolean isLeapYear(int year) {
		return new GregorianCalendar().isLeapYear(year);
	}
	
	public static final int getDaysInYear(int year) {
		return isLeapYear(year) ? 366 : 365;
	}
	
	/**
	 * Returns a date with a number of hours added to it.
	 * 
	 * @param date	The date to add to.
	 * @param numberOfHours The number of hours to add (This can be negative).
	 */
	public static final void addHours(Date date, int numberOfHours) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		calendar.setLenient(false);

		// NB clear() does not work in JDK 1.3.1
		calendar.add(Calendar.HOUR_OF_DAY, numberOfHours);

		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * Returns a date with a number of days added to it.
	 * 
	 * @param date	The date to add to.
	 * @param numberOfDays The number of days to add (This can be negative).
	 */
	public static final void addDays(Date date, int numberOfDays) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		calendar.setLenient(false);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);

		calendar.add(Calendar.DATE, numberOfDays);

		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * Returns a date with a number of months added to it.
	 * 
	 * @param date	The date to add months to.
	 * @param numberOfMonths The number of months to add (This can be negative).
	 */
	public static final void addMonths(Date date, int numberOfMonths) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		calendar.setLenient(false);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);

		calendar.add(Calendar.MONTH, numberOfMonths);

		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * Returns the Financial Year in which this date exists.
	 */
	public static final int getFinancialYear(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		int year = calendar.get(Calendar.YEAR);
		int month = calendar.get(Calendar.MONTH);

		if (month <= 5) {
			year--;
		}

		return year;
	}

	/**
	 * Returns the Financial Year String in which this date exists.
	 */
	public static final String getFinancialYearString(Date date) {
		int financialYear = getFinancialYear(date);

		return financialYear + "/" + (financialYear + 1);
	}

	public static final Decimal5 numberOfHoursBetween(TimeOnly start, TimeOnly end) {
		long minutesBetween = (end.getTime() - start.getTime()) / 60000;
		Decimal5 result = new Decimal5(minutesBetween);
		result = result.divide(Decimal5.SIXTY);
		return result;
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
		Calendar startDateCalendar = new GregorianCalendar();
		startDateCalendar.setTime(startDate);
		startDateCalendar.setLenient(false);
		// NB clear() does not work in JDK 1.3.1
		startDateCalendar.set(Calendar.MILLISECOND, 0);
		startDateCalendar.set(Calendar.SECOND, 0);
		startDateCalendar.set(Calendar.MINUTE, 0);
		startDateCalendar.set(Calendar.HOUR_OF_DAY, 0);

		Calendar endDateCalendar = new GregorianCalendar();
		endDateCalendar.setTime(endDate);
		endDateCalendar.setLenient(false);
		// NB clear() does not work in JDK 1.3.1
		endDateCalendar.set(Calendar.MILLISECOND, 0);
		endDateCalendar.set(Calendar.SECOND, 0);
		endDateCalendar.set(Calendar.MINUTE, 0);
		endDateCalendar.set(Calendar.HOUR_OF_DAY, 0);

		// Calculate the days between startDate and endDate
		int daysBetweenStartDateAndEndDate = 0;
		if (startDate.after(endDate)) {
			while (startDateCalendar.after(endDateCalendar)) {
				startDateCalendar.add(Calendar.DATE, -1);
				daysBetweenStartDateAndEndDate--;
			}
		}
		else {
			while (startDateCalendar.before(endDateCalendar)) {
				startDateCalendar.add(Calendar.DATE, 1);
				daysBetweenStartDateAndEndDate++;
			}
		}

		return daysBetweenStartDateAndEndDate;
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
		int numberOfDays = numberOfDaysBetween(startDate, endDate);
		// now add 1 to the absolute value of the days between to give the
		// total number of days included in the range (because the number
		// of days between 1/7/1970 and 2/7/1970 is 1, but the total number
		// of days in the range is 2).
		if (numberOfDays >= 0) {
			numberOfDays++;
		}
		else {
			numberOfDays--;
		}

		return numberOfDays;
	}

	/**
	 * Find the next day of the week.
	 * 
	 * @param date The existing date to roll.
	 * @param dayOfWeek The int representing the day of the week - from {@link Calendar}. eg Calendar.MONDAY
	 * @return	The date passed in to allow method chaining.
	 */
	public static final Date findNextDayOfWeek(Date date, int dayOfWeek) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		while (calendar.get(Calendar.DAY_OF_WEEK) != dayOfWeek) {
			calendar.add(Calendar.DAY_OF_WEEK, 1);
		}

		date.setTime(calendar.getTime().getTime());
		
		return date;
	}
	
	/**
	 * Find the next day of the week.
	 * 
	 * @param date The existing date to roll.
	 * @param dayOfWeek The int representing the day of the week - from {@link Calendar}. eg Calendar.MONDAY
	 * @return	The date passed in to allow method chaining.
	 */
	public static final Date ensureWorkDay(Date date) {
		Calendar calendar = new GregorianCalendar();
		calendar.setTime(date);
		int dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK);
		if (dayOfWeek == Calendar.SATURDAY) {
			calendar.add(Calendar.DAY_OF_WEEK, 2);
			date.setTime(calendar.getTime().getTime());
		}
		else if (dayOfWeek == Calendar.SUNDAY) {
			calendar.add(Calendar.DAY_OF_WEEK, 1);
			date.setTime(calendar.getTime().getTime());
		}

		return date;
	}
	
	/**
	 * Output a description of the days between 2 dates
	 * @param now	The reference date to compare.
	 * @param agoOrUntil	When to compare the now date to.
	 * @return	"Today", "Yesterday", "Tomorrow" or "? days ago" or "? days time".
	 */
	public static final String daysBetweenDescription(Date now, Date agoOrUntil) {
		int daysBetween = numberOfDaysBetween(now, agoOrUntil);
		
		if (daysBetween == 0) {
			return "Today";
		}
		if (daysBetween == 1) {
			return "Tomorrow";
		}
		if (daysBetween == -1) {
			return "Yesterday";
		}
		
		StringBuilder result = new StringBuilder(32).append(Math.abs(daysBetween));
		result.append((daysBetween < 0) ? " days ago" : " days time");
		return result.toString();
	}
	
	public static Date parseISODate(String date) throws ParseException {
		String param = date;
		int l = date.length();
		if (l == 23) { // cater for no timezone = UTC/GMT/Zulu
			param = date + "+00:00";
		}
		else if (l == 24) { // cater for a 'Z' = Zulu
			param = date.substring(0, 23) + "+00:00";
		}
		if (l == 19) { // cater for no millis and timezone = UTC/GMT/Zulu
			param = date + ".000+00:00";
		}
		else if (l == 20) { // cater for no millis and a 'Z' = Zulu
			param = date.substring(0, 19) + ".000+00:00";
		}
		return DateUtils.parseDate(param, new String[]{ "yyyy-MM-dd'T'HH:mm:ss.SSSZZ" });
	}

	public static String formatISODate(Date date, boolean inUTC) {
		if (inUTC) {
			return DateFormatUtils.format(date, "yyyy-MM-dd'T'HH:mm:ss.SSSZZ", TimeZone.getTimeZone("UTC"));
		}
		return DateFormatUtils.format(date, "yyyy-MM-dd'T'HH:mm:ss.SSSZZ");
	}
}
