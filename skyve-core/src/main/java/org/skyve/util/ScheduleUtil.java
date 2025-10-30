package org.skyve.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Central scheduling enums & helpers.
 * Provides:
 * <ul>
 *   <li>{@link OccurrenceFrequency}, {@link DayOfWeek}, {@link OccurrencePeriod}</li>
 *   <li>Immutable {@link DomainValue} lists for UI (frequencies, term frequencies, week days, periods)</li>
 *   <li>Helpers: calendar day conversion, annual occurrence counts, frequency-based date addition</li>
 * </ul>
 * Notes: annual counts are approximate; lists should be treated as read‑only.
 */
public class ScheduleUtil {
	/** general types of time-based frequencies */
	public static enum OccurrenceFrequency {
		OneOff, EverySecond, EveryMinute, Hourly, Daily, Weekly, Fortnightly, Monthly, Quarterly, HalfYearly, Yearly, Irregularly, DuringHolidays, NotDuringHolidays, WeekDays, Weekends;
	}

	public static final List<DomainValue> OCCURRENCE_FREQUENCIES = new ArrayList<>();

	static {
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Irregularly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.OneOff.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Hourly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Daily.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Weekly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Fortnightly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Monthly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Quarterly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.HalfYearly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Yearly.toString()));
	}

	/** subset of frequencies relevant for use as terms */
	public static final List<DomainValue> TERM_FREQUENCIES = new ArrayList<>();

	static {
		TERM_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Irregularly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Weekly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Fortnightly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Monthly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Quarterly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.HalfYearly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurrenceFrequency.Yearly.toString()));
	}

	/** normal days of the week */
	public static enum DayOfWeek {
		Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday
	}

	public static final List<DomainValue> WEEK_DAYS = new ArrayList<>();

	static {
		WEEK_DAYS.add(new DomainValue(DayOfWeek.Sunday.toString()));
		WEEK_DAYS.add(new DomainValue(DayOfWeek.Monday.toString()));
		WEEK_DAYS.add(new DomainValue(DayOfWeek.Tuesday.toString()));
		WEEK_DAYS.add(new DomainValue(DayOfWeek.Wednesday.toString()));
		WEEK_DAYS.add(new DomainValue(DayOfWeek.Thursday.toString()));
		WEEK_DAYS.add(new DomainValue(DayOfWeek.Friday.toString()));
		WEEK_DAYS.add(new DomainValue(DayOfWeek.Saturday.toString()));
	}

	/** returns the number of days between day1 and day2 */
	public static enum OccurrencePeriod {
		Seconds, Minutes, Hours, Days, Weeks, Months, Years
	}

	public static final List<DomainValue> OCCURRENCE_PERIODS = new ArrayList<>();

	static {
		OCCURRENCE_PERIODS.add(new DomainValue(OccurrencePeriod.Seconds.toString()));
		OCCURRENCE_PERIODS.add(new DomainValue(OccurrencePeriod.Minutes.toString()));
		OCCURRENCE_PERIODS.add(new DomainValue(OccurrencePeriod.Hours.toString()));
		OCCURRENCE_PERIODS.add(new DomainValue(OccurrencePeriod.Days.toString()));
		OCCURRENCE_PERIODS.add(new DomainValue(OccurrencePeriod.Weeks.toString()));
		OCCURRENCE_PERIODS.add(new DomainValue(OccurrencePeriod.Months.toString()));
		OCCURRENCE_PERIODS.add(new DomainValue(OccurrencePeriod.Years.toString()));
	}
	
	/**
	 * Returns a calendar day of the week
	 *
	 * @param weekDay
	 *        - the day of the week (DayOfWeek)
	 * @return - the day of the week as a Calendar.day (int)
	 */
	public static final int dayOfWeekToCalendar(DayOfWeek weekDay) {
		int calendarDay = Calendar.MONDAY;

		if (weekDay.equals(DayOfWeek.Monday)) {
			calendarDay = Calendar.MONDAY;
		}
		if (weekDay.equals(DayOfWeek.Tuesday)) {
			calendarDay = Calendar.TUESDAY;
		}
		if (weekDay.equals(DayOfWeek.Wednesday)) {
			calendarDay = Calendar.WEDNESDAY;
		}
		if (weekDay.equals(DayOfWeek.Thursday)) {
			calendarDay = Calendar.THURSDAY;
		}
		if (weekDay.equals(DayOfWeek.Friday)) {
			calendarDay = Calendar.FRIDAY;
		}
		if (weekDay.equals(DayOfWeek.Saturday)) {
			calendarDay = Calendar.SATURDAY;
		}
		if (weekDay.equals(DayOfWeek.Sunday)) {
			calendarDay = Calendar.SUNDAY;
		}

		return calendarDay;
	}

	/**
	 * Returns a day of the week from a Calendar day
	 *
	 * @param calendarDay
	 *        - the number of the day (int)
	 * @return - the DayOfWeek (DayOfWeek)
	 */
	public static final DayOfWeek calendarToDayOfWeek(int calendarDay) {
		DayOfWeek weekDay = DayOfWeek.Monday;

		if (calendarDay == Calendar.MONDAY) {
			weekDay = DayOfWeek.Monday;
		}
		if (calendarDay == Calendar.TUESDAY) {
			weekDay = DayOfWeek.Tuesday;
		}
		if (calendarDay == Calendar.WEDNESDAY) {
			weekDay = DayOfWeek.Wednesday;
		}
		if (calendarDay == Calendar.THURSDAY) {
			weekDay = DayOfWeek.Thursday;
		}
		if (calendarDay == Calendar.FRIDAY) {
			weekDay = DayOfWeek.Friday;
		}
		if (calendarDay == Calendar.SATURDAY) {
			weekDay = DayOfWeek.Saturday;
		}
		if (calendarDay == Calendar.SUNDAY) {
			weekDay = DayOfWeek.Sunday;
		}

		return weekDay;
	}

	/**
	 * Returns the number of periods of specified frequency which occur in the
	 * calendar year.
	 *
	 * @param frequency
	 *        - the specified frequency (OccurrenceFrequency)
	 * @return - the number of times the specified frequency occurs in a
	 *         calendar year
	 */
	public static int annualFrequencyCount(OccurrenceFrequency frequency) {
		int periodCount = 1; // default period Count

		if (frequency.equals(OccurrenceFrequency.Daily)) {
			// estimated
			periodCount = 365;
		} else if (frequency.equals(OccurrenceFrequency.Weekly)) {
			periodCount = 52;
		} else if (frequency.equals(OccurrenceFrequency.Fortnightly)) {
			periodCount = 26;
		} else if (frequency.equals(OccurrenceFrequency.Monthly)) {

			periodCount = 12;
		} else if (frequency.equals(OccurrenceFrequency.Quarterly)) {
			periodCount = 4;
		} else if (frequency.equals(OccurrenceFrequency.HalfYearly)) {
			periodCount = 2;
		}

		return periodCount;
	}

	/**
	 * Returns the number of periods which occur in a calendar year.
	 *
	 * @param period
	 *        - the time period (OccurrencePeriod)
	 * @return - the number of times the period occurs within a calendar year
	 *         (int)
	 */
	public static int annualPeriodCount(OccurrencePeriod period) {
		int periodCount = 1; // default period Count

		if (period.equals(OccurrencePeriod.Days)) {
			// estimated
			periodCount = 365;
		} else if (period.equals(OccurrencePeriod.Weeks)) {
			periodCount = 52;
		} else if (period.equals(OccurrencePeriod.Months)) {
			periodCount = 12;
		} else if (period.equals(OccurrencePeriod.Years)) {
			periodCount = 1;
		}

		return periodCount;
	}

	/**
	 * Adds a time frequency to a given date.
	 *
	 * @param frequency
	 *        - the frequency to add
	 * @param date
	 *        - the date to add to
	 * @param numberOfFrequencies
	 *        - the number of frequencies to add
	 * @return - the resulting date
	 */
	public static final DateOnly addFrequency(OccurrenceFrequency frequency, DateOnly date, int numberOfFrequencies) {
		if (date != null) {
			if (frequency.equals(OccurrenceFrequency.OneOff)) {
				return new DateOnly(date.getTime());
			}

			DateOnly newDate = new DateOnly(date.getTime());
			Calendar calendar = Calendar.getInstance();
			calendar.setTime(newDate);
			calendar.setLenient(false);

			// NB clear() does not work in JDK 1.3.1
			calendar.set(Calendar.MILLISECOND, 0);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MINUTE, 0);
			calendar.set(Calendar.HOUR_OF_DAY, 0);

			if (frequency.equals(OccurrenceFrequency.Daily)) {
				calendar.add(Calendar.DATE, numberOfFrequencies);
			} else if (frequency.equals(OccurrenceFrequency.Weekly)) {
				calendar.add(Calendar.DATE, (numberOfFrequencies * 7));
			} else if (frequency.equals(OccurrenceFrequency.Fortnightly)) {
				calendar.add(Calendar.DATE, (numberOfFrequencies * 14));
			} else if (frequency.equals(OccurrenceFrequency.Monthly)) {
				calendar.add(Calendar.MONTH, numberOfFrequencies);
			} else if (frequency.equals(OccurrenceFrequency.Quarterly)) {
				calendar.add(Calendar.MONTH, (numberOfFrequencies * 3));
			} else if (frequency.equals(OccurrenceFrequency.HalfYearly)) {
				calendar.add(Calendar.MONTH, (numberOfFrequencies * 6));
			} else if (frequency.equals(OccurrenceFrequency.Yearly)) {
				calendar.add(Calendar.YEAR, numberOfFrequencies);
			}

			newDate.setTime(calendar.getTime().getTime());

			return newDate;
		}
		return null;
	}
}
