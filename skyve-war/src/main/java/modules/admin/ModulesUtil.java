package modules.admin;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.MathUtil;
import org.skyve.util.MonthDomainValues;
import org.skyve.util.ScheduleUtil;

/**
 * Utility methods applicable across application modules.
 * <p>
 * This class is provided as part of Skyve
 *
 * @author robert.brown
 *
 */
public class ModulesUtil {

	/**
	 * @deprecated Use {@link org.skyve.util.Util#MEGABYTE} instead.
	 */
	@Deprecated
	public static final long MEGABYTE = org.skyve.util.Util.MEGABYTE;

	/**
	 * @deprecated Use {@link ScheduleUtil.OccurrenceFrequency} and {@link ScheduleUtil#OCCURRENCE_FREQUENCIES} instead.
	 */
	@Deprecated
	public static enum OccurenceFrequency {
		OneOff, EverySecond, EveryMinute, Hourly, Daily, Weekly, Fortnightly, Monthly, Quarterly, HalfYearly, Yearly, Irregularly, DuringHolidays, NotDuringHolidays, WeekDays, Weekends
	}

	/**
	 * @deprecated Use {@link ScheduleUtil#OCCURRENCE_FREQUENCIES} instead.
	 */
	@Deprecated
	public static final List<DomainValue> OCCURRENCE_FREQUENCIES = ScheduleUtil.OCCURRENCE_FREQUENCIES;

	/**
	 * @deprecated Use {@link ScheduleUtil#TERM_FREQUENCIES} instead.
	 */
	@Deprecated
	public static final List<DomainValue> TERM_FREQUENCIES = ScheduleUtil.TERM_FREQUENCIES;

	/**
	 * @deprecated Use {@link ScheduleUtil.DayOfWeek} and {@link ScheduleUtil#WEEK_DAYS} instead.
	 */
	@Deprecated
	public static enum DayOfWeek {
		Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday
	}

	/**
	 * @deprecated Use {@link ScheduleUtil#WEEK_DAYS} instead.
	 */
	@Deprecated
	public static final List<DomainValue> WEEK_DAYS = ScheduleUtil.WEEK_DAYS;

	/**
	 * @deprecated Use {@link ScheduleUtil.OccurrencePeriod} and {@link ScheduleUtil#OCCURRENCE_PERIODS} instead.
	 */
	@Deprecated
	public static enum OccurrencePeriod {
		Seconds, Minutes, Hours, Days, Weeks, Months, Years
	}

	/**
	 * @deprecated Use {@link ScheduleUtil#OCCURRENCE_PERIODS} instead.
	 */
	@Deprecated
	public static final List<DomainValue> OCCURRENCE_PERIODS = ScheduleUtil.OCCURRENCE_PERIODS;

	/**
	 * @deprecated Use {@link ScheduleUtil#dayOfWeekToCalendar(ScheduleUtil.DayOfWeek)} instead.
	 */
	@Deprecated
	public static int dayOfWeekToCalendar(DayOfWeek weekDay) {
		return ScheduleUtil.dayOfWeekToCalendar(ScheduleUtil.DayOfWeek.valueOf(weekDay.name()));
	}

	/**
	 * @deprecated Use {@link ScheduleUtil#calendarToDayOfWeek(int)} instead.
	 */
	@Deprecated
	public static DayOfWeek calendarToDayOfWeek(int calendarDay) {
		return DayOfWeek.valueOf(ScheduleUtil.calendarToDayOfWeek(calendarDay).name());
	}

	/**
	 * @deprecated Use {@link ScheduleUtil#annualFrequencyCount(ScheduleUtil.OccurrenceFrequency)} instead.
	 */
	@Deprecated
	public static int annualFrequencyCount(OccurenceFrequency frequency) {
		return ScheduleUtil.annualFrequencyCount(ScheduleUtil.OccurrenceFrequency.valueOf(frequency.name()));
	}

	/**
	 * @deprecated Use {@link ScheduleUtil#annualPeriodCount(ScheduleUtil.OccurrencePeriod)} instead.
	 */
	@Deprecated
	public static int annualPeriodCount(OccurrencePeriod period) {
		return ScheduleUtil.annualPeriodCount(ScheduleUtil.OccurrencePeriod.valueOf(period.name()));
	}

	/**
	 * @deprecated Use {@link ScheduleUtil#addFrequency(ScheduleUtil.OccurrenceFrequency, DateOnly, int)} instead.
	 */
	@Deprecated
	public static DateOnly addFrequency(OccurenceFrequency frequency, DateOnly date, int numberOfFrequencies) {
		return ScheduleUtil.addFrequency(ScheduleUtil.OccurrenceFrequency.valueOf(frequency.name()), date, numberOfFrequencies);
	}

	/**
	 * Returns the last day of the month in which the specified date occurs.
	 *
	 * @param date
	 *        - the specified date
	 * @return - the date of the last day of the month in which the specified
	 *         date occurs
	 */
	/**
	 * @deprecated Use {@link org.skyve.util.Time#lastDayOfMonth(DateOnly)} instead.
	 */
	@Deprecated
	public static DateOnly lastDayOfMonth(DateOnly date) {
		return org.skyve.util.Time.lastDayOfMonth(date);
	}

	/**
	 * Returns the last day of the year in which the specified date occurs.
	 *
	 * @param date
	 *        - the specified date
	 * @return - the date of the last day of the year in which the specified
	 *         date occurs
	 */
	/**
	 * @deprecated Use {@link org.skyve.util.Time#lastDayOfYear(DateOnly)} instead.
	 */
	@Deprecated
	public static DateOnly lastDayOfYear(DateOnly date) {
		return org.skyve.util.Time.lastDayOfYear(date);
	}

	/**
	 * Returns the date of the first day of the month in which the specified
	 * date occurs.
	 *
	 * @param date
	 *        - the specified date
	 * @return - the date of the first day of that month
	 */
	/**
	 * @deprecated Use {@link org.skyve.util.Time#firstDayOfMonth(DateOnly)} instead.
	 */
	@Deprecated
	public static DateOnly firstDayOfMonth(DateOnly date) {
		return org.skyve.util.Time.firstDayOfMonth(date);
	}

	/**
	 * Returns the date of the first day of the year in which the specified date
	 * occurs.
	 *
	 * @param date
	 *        - the specified date
	 * @return - the date of the first day of that year
	 */
	/**
	 * @deprecated Use {@link org.skyve.util.Time#firstDayOfYear(DateOnly)} instead.
	 */
	@Deprecated
	public static DateOnly firstDayOfYear(DateOnly date) {
		return org.skyve.util.Time.firstDayOfYear(date);
	}

	/**
	 * Returns the date which occurs after the specified date, given the number
	 * of days to add.
	 *
	 * @param date
	 *        - the specified date
	 * @param daysToAdd
	 *        - the number of days to add to that date
	 * @return - the resulting date
	 */
	/**
	 * @deprecated Use {@link org.skyve.util.Time#plusDays(DateOnly, int)} instead.
	 */
	@Deprecated
	public static DateOnly addDaysDateOnly(DateOnly date, int daysToAdd) {
		return org.skyve.util.Time.plusDays(date, daysToAdd);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.Time#sqlFormatDateOnly(DateOnly)} or (preferably) parameter binding instead.
	 */
	@Deprecated
	public static String sqlFormatDateOnly(DateOnly theDate) {
		return org.skyve.util.Time.sqlFormatDateOnly(theDate);
	}

	/** Returns a TitleCase version of the String supplied */
	/**
	 * @deprecated Use {@link org.skyve.util.StringUtil#titleCase(String)} instead.
	 */
	@Deprecated
	public static String titleCase(String raw) {
		return org.skyve.util.StringUtil.titleCase(raw);
	}

	/**
	 * @deprecated Use {@link MonthDomainValues#CALENDAR_MONTHS} instead.
	 */
	@Deprecated
	public static final List<DomainValue> CALENDAR_MONTHS = MonthDomainValues.CALENDAR_MONTHS;

	/**
	 * @deprecated Use {@link MonthDomainValues#monthNameToZeroBasedIndex(String)} instead.
	 */
	@Deprecated
	public static int calendarMonthNameToNumber(String monthName) {
		int result = MonthDomainValues.monthNameToZeroBasedIndex(monthName);
		return (result < 0) ? 0 : result; // preserve legacy fallback behaviour
	}

	/**
	 * @deprecated Use {@link org.skyve.util.Util#addValidationError(ValidationException, String, String)} instead.
	 */
	@Deprecated
	public static void addValidationError(ValidationException e, String fieldName, String messageString) {
		org.skyve.util.Util.addValidationError(e, fieldName, messageString);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.MathUtil#getUniqueQuadraticResidue(long)} instead.
	 */
	@Deprecated
	public static long getUniqueQuadraticResidue(long incrementingNumber) {
		return MathUtil.getUniqueQuadraticResidue(incrementingNumber);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.Util#getConditionName(String)} instead.
	 */
	@Deprecated
	public static String getConditionName(String conditionCode) {
		return org.skyve.util.Util.getConditionName(conditionCode);
	}

	/** allows comparison where both terms being null evaluates as equality */
	/**
	 * @deprecated Use {@link java.util.Objects#equals(Object, Object)} instead.
	 */
	@Deprecated
	public static boolean bothNullOrEqual(Object object1, Object object2) {
		return java.util.Objects.equals(object1, object2);
	}

	/**
	 * Replaces the value found in the bean for the binding string provided,
	 * e.g. if the bean has a binding of contact.name, for which the
	 * displayNames of those bindings are Contact.FullName , then get the value
	 * of that binding from the bean provided.
	 *
	 * @param bean
	 *        - the bean relevant for the binding
	 * @param replacementString
	 *        - the string representing the displayName form of the binding
	 * @return - the value from the bean
	 * @throws Exception
	 *         general Exception for metadata exception or string
	 *         manipulation failure etc
	 */
	/**
	 * @deprecated Use {@link org.skyve.util.StringUtil#replaceBindingsInString(Bean, String)} instead.
	 */
	@Deprecated
	public static String replaceBindingsInString(Bean bean, String replacementString) throws Exception {
		return org.skyve.util.StringUtil.replaceBindingsInString(bean, replacementString);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.StringUtil#concatWithDelim(String, String...)} instead.
	 */
	@Deprecated
	public static String concatWithDelim(String delimiter, String... strings) {
		return org.skyve.util.StringUtil.concatWithDelim(delimiter, strings);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.StringUtil#enquote(String, String)} instead.
	 */
	@Deprecated
	public static String enquote(String quoteSet, String s) {
		return org.skyve.util.StringUtil.enquote(quoteSet, s);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.Util#lookupBean(String, String, String, Object)} instead.
	 */
	@Deprecated
	public static Bean lookupBean(String moduleName, String documentName, String propertyName, Object objValue) {
		return org.skyve.util.Util.lookupBean(moduleName, documentName, propertyName, objValue);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.Util#getPersistentIdentifier(String, String)} instead.
	 */
	@Deprecated
	public static String getPersistentIdentifier(final String moduleName, final String documentName) {
		return org.skyve.util.Util.getPersistentIdentifier(moduleName, documentName);
	}

	/**
	 * @deprecated Use {@link org.skyve.util.Util#getCompleteSuggestions(String, String, String, String)} instead.
	 */
	@Deprecated
	public static List<String> getCompleteSuggestions(String moduleName, String documentName, String attributeName, String value)
			throws Exception {
		return org.skyve.util.Util.getCompleteSuggestions(moduleName, documentName, attributeName, value);
	}
}
