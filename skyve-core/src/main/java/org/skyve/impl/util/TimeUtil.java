package org.skyve.impl.util;

import java.text.ParseException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.Year;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.util.Time;

public class TimeUtil {
	private TimeUtil() {
		// no implementation
	}

	/**
	 * See {@link Time#clearTimeComponent}
	 */
	public static final void clearTimeComponent(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * See {@link Time#clearMillisecondComponent}
	 */
	public static final void clearMillisecondComponent(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		date.setTime(calendar.getTime().getTime());
	} // clearTimeComponent

	/**
	 * See {@link Time#clearDateComponent}
	 */
	public static void clearDateComponent(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.DAY_OF_MONTH, 1);
		calendar.set(Calendar.MONTH, 0);
		calendar.set(Calendar.YEAR, 1970);
		date.setTime(calendar.getTime().getTime());
	}
	
	/**
	 * See {@link Time#setTime}
	 */
	public static final void setTime(Date date, int hours24, int minutes, int seconds) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, seconds);
		calendar.set(Calendar.MINUTE, minutes);
		calendar.set(Calendar.HOUR_OF_DAY, hours24);
		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * See {@link Time#setTime}
	 */
	public static final void setTime(Date date, TimeOnly time) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(time);
		int hours = calendar.get(Calendar.HOUR_OF_DAY);
		int minutes= calendar.get(Calendar.MINUTE);
		int seconds=  calendar.get(Calendar.SECOND);
		//int millis = calendar.get(Calendar.MILLISECOND);
		
		setTime(date, hours, minutes, seconds);
	}
	
	/**
	 * See {@link Time#setDate}
	 */
	public static final void setDate(Date date, int dayOfMonth, int monthStartingAt1, int year) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.set(Calendar.DAY_OF_MONTH, dayOfMonth);
		calendar.set(Calendar.MONTH, monthStartingAt1 - 1);
		calendar.set(Calendar.YEAR, year);
		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * See {@link Time#getYear}
	 */
	public static final int getYear(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);

		return calendar.get(Calendar.YEAR);
	}

	/**
	 * See {@link Time#getMonthStartingFrom1}
	 */
	public static final int getMonthStartingFrom1(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);

		return calendar.get(Calendar.MONTH) + 1;
	}

	/**
	 * See {@link Time#getMonthStartingFrom0}
	 */
	public static final int getMonthStartingFrom0(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);

		return calendar.get(Calendar.MONTH);
	}

	/**
	 * See {@link Time#getDay}
	 */
	public static final int getDay(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);

		return calendar.get(Calendar.DAY_OF_MONTH);
	}

	/**
	 * See {@link Time#isLeapYear}
	 */
	public static final boolean isLeapYear(int year) {
		return Year.isLeap(year);
	}
	
	/**
	 * See {@link Time#getDaysInYear}
	 */
	public static final int getDaysInYear(int year) {
		return isLeapYear(year) ? 366 : 365;
	}
	
	/**
	 * See {@link Time#addHours}
	 */
	public static final void addHours(Date date, int numberOfHours) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.setLenient(false);

		// NB clear() does not work in JDK 1.3.1
		calendar.add(Calendar.HOUR_OF_DAY, numberOfHours);

		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * See {@link Time#addMinutes}
	 */
	public static final void addMinutes(Date date, int numberOfMinutes) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.setLenient(false);

		// NB clear() does not work in JDK 1.3.1
		calendar.add(Calendar.MINUTE, numberOfMinutes);

		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * See {@link Time#addSeconds}
	 */
	public static final void addSeconds(Date date, int numberOfSeconds) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.setLenient(false);

		// NB clear() does not work in JDK 1.3.1
		calendar.add(Calendar.SECOND, numberOfSeconds);

		date.setTime(calendar.getTime().getTime());
	}

	/**
	 * See {@link Time#addDays}
	 */
	public static final void addDays(Date date, int numberOfDays) {
		Calendar calendar = Calendar.getInstance();
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
	 * See {@link Time#addMonths}
	 */
	public static final void addMonths(Date date, int numberOfMonths) {
		Calendar calendar = Calendar.getInstance();
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
	 * See {@link Time#getFinancialYear}
	 */
	public static final int getFinancialYear(Date date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		int year = calendar.get(Calendar.YEAR);
		int month = calendar.get(Calendar.MONTH);

		if (month <= Calendar.JUNE) {
			year--;
		}

		return year;
	}

	/**
	 * See {@link Time#getFinancialYearString}
	 */
	public static final String getFinancialYearString(Date date) {
		int financialYear = getFinancialYear(date);

		return financialYear + "/" + (financialYear + 1);
	}

	/**
	 * See {@link Time#numberOfHoursBetween}
	 */
	public static final Decimal5 numberOfHoursBetween(TimeOnly start, TimeOnly end) {
		long minutesBetween = (end.getTime() - start.getTime()) / 60000;
		Decimal5 result = new Decimal5(minutesBetween);
		result = result.divide(Decimal5.SIXTY);
		return result;
	}

	/**
	 * See {@link Time#numberOfDaysBetween}
	 */
	public static final int numberOfDaysBetween(Date startDate, Date endDate) {
		Calendar startDateCalendar = Calendar.getInstance();
		startDateCalendar.setTime(startDate);
		startDateCalendar.setLenient(false);
		// NB clear() does not work in JDK 1.3.1
		startDateCalendar.set(Calendar.MILLISECOND, 0);
		startDateCalendar.set(Calendar.SECOND, 0);
		startDateCalendar.set(Calendar.MINUTE, 0);
		startDateCalendar.set(Calendar.HOUR_OF_DAY, 0);

		Calendar endDateCalendar = Calendar.getInstance();
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
	 * See {@link Time#numberOfDaysInRange}
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
	 * See {@link Time#findNextDayOfWeek}
	 */
	public static final Date findNextDayOfWeek(Date date, int dayOfWeek) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		while (calendar.get(Calendar.DAY_OF_WEEK) != dayOfWeek) {
			calendar.add(Calendar.DAY_OF_WEEK, 1);
		}

		date.setTime(calendar.getTime().getTime());
		
		return date;
	}
	
	/**
	 * See {@link Time#ensureWorkDay}
	 */
	public static final Date ensureWorkDay(Date date) {
		Calendar calendar = Calendar.getInstance();
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
	 * See {@link Time#daysBetweenDescription}
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
	
	/**
	 * See {@link Time#addDaysToNew}
	 */
	public static DateOnly addDaysToNew(final Date date, final int numberOfDays) {

		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}

		final Calendar calendar = Calendar.getInstance();
		calendar.setLenient(false);
		calendar.setTime(date);
		calendar.add(Calendar.DAY_OF_MONTH, numberOfDays);
		return new DateOnly(calendar.getTime());
	}

	/**
	 * See {@link Time#addMonthsToNew}
	 */
	public static DateOnly addMonthsToNew(final Date date, final int numberOfMonths) {

		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}

		final Calendar calendar = Calendar.getInstance();
		calendar.setLenient(false);
		calendar.setTime(date);
		calendar.add(Calendar.MONTH, numberOfMonths);
		return new DateOnly(calendar.getTime());
	}

	/**
	 * See {@link Time#addYearsToNew}
	 */
	public static DateOnly addYearsToNew(final Date date, final int numberOfYears) {

		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}

		final Calendar calendar = Calendar.getInstance();
		calendar.setLenient(false);
		calendar.setTime(date);
		calendar.add(Calendar.YEAR, numberOfYears);
		return new DateOnly(calendar.getTime());
	}

	/**
	 * See {@link Time#asDateOnly}
	 */
	public static DateOnly asDateOnly(LocalDate localDate) {
		return new DateOnly(Date.from(localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant()));
	}

	/**
	 * See {@link Time#asLocalDate}
	 */
	public static LocalDate asLocalDate(Date date) {
		return Instant.ofEpochMilli(date.getTime()).atZone(ZoneId.systemDefault()).toLocalDate();
	}

	/**
	 * See {@link Time#coalesce}
	 */
	public static DateOnly coalesce(DateOnly val, DateOnly ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/**
	 * See {@link Time#min}
	 */
	public static DateOnly min(final DateOnly... dates) {
		if (dates == null || dates.length == 0) {
			throw new IllegalArgumentException("At least one date must be supplied");
		}

		List<DateOnly> dateList = Arrays.asList(dates);
		return Collections.min(dateList);
	}

	/**
	 * See {@link Time#max}
	 */
	public static DateOnly max(final DateOnly... dates) {
		if (dates == null || dates.length == 0) {
			throw new IllegalArgumentException("At least one date must be supplied");
		}

		List<DateOnly> dateList = Arrays.asList(dates);
		return Collections.max(dateList);
	}

	/**
	 * See {@link Time#withDate}
	 */
	public static DateOnly withDate(int dayOfMonth, int monthStartingAt1, int year) {
		DateOnly date = new DateOnly();
		TimeUtil.setDate(date, dayOfMonth, monthStartingAt1, year);
		return date;
	}
}
