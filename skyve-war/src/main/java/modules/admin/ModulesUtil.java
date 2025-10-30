package modules.admin;

import java.util.Comparator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.MonthDomainValues;
import org.skyve.util.ScheduleUtil;

import modules.admin.Group.GroupExtension;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

/**
 * Utility methods applicable across application modules.
 * <p>
 * This class is provided as part of Skyve
 *
 * @author robert.brown
 *
 */
public class ModulesUtil {

	public static final long MEGABYTE = 1024L * 1024L;

	/** comparator to allow sorting of domain values by code */
	public static class DomainValueSortByCode implements Comparator<DomainValue> {
		@Override
		public int compare(DomainValue d1, DomainValue d2) {
			return d1.getCode().compareTo(d2.getCode());
		}
	}

	/** comparator to allow sorting of domain values by description */
	public static class DomainValueSortByDescription implements Comparator<DomainValue> {
		@Override
		public int compare(DomainValue d1, DomainValue d2) {
			return d1.getLocalisedDescription().compareTo(d2.getLocalisedDescription());
		}
	}
	
	/**
	 * @deprecated Use {@link ScheduleUtil.OccurrenceFrequency} and {@link ScheduleUtil#OCCURRENCE_FREQUENCIES} instead.
	 */
	@Deprecated
	public static enum OccurenceFrequency { OneOff, EverySecond, EveryMinute, Hourly, Daily, Weekly, Fortnightly, Monthly, Quarterly, HalfYearly, Yearly, Irregularly, DuringHolidays, NotDuringHolidays, WeekDays, Weekends }

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
	public static enum DayOfWeek { Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday }

	/**
	 * @deprecated Use {@link ScheduleUtil#WEEK_DAYS} instead.
	 */
	@Deprecated
	public static final List<DomainValue> WEEK_DAYS = ScheduleUtil.WEEK_DAYS;

	/**
	 * @deprecated Use {@link ScheduleUtil.OccurrencePeriod} and {@link ScheduleUtil#OCCURRENCE_PERIODS} instead.
	 */
	@Deprecated
	public static enum OccurrencePeriod { Seconds, Minutes, Hours, Days, Weeks, Months, Years }

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

	public static void addValidationError(ValidationException e, String fieldName, String messageString) {
		Message vM = new Message(messageString);
		vM.addBinding(fieldName);
		e.getMessages().add(vM);
	}

	private static final long PRIME = 4294967291L;
	private static final long HALF_PRIME = PRIME / 2L;

	/**
	 * Taking in a incrementing integer this function will create a fairly uniformly distributed,
	 * sparse and unique set of numbers for inputs less than the prime (4,294,967,291).
	 * See https://en.wikipedia.org/wiki/Quadratic_residue
	 * 
	 * @param incrementingNumber The number to generate a unique pseudo random number for
	 * @return The quadratic residue.
	 */
	public static long getUniqueQuadraticResidue(long incrementingNumber) {
		long x = incrementingNumber + 1001; // for sufficient entropy
		long residue = (x * x) % PRIME;
		return (x <= HALF_PRIME) ? residue : (PRIME - residue);
	}

	/** returns a formatted string representing the condition */
	public static String getConditionName(String conditionCode) {
		String result = "is";

		result += conditionCode.substring(0, 1).toUpperCase() + conditionCode.substring(1, conditionCode.length());
		// System.out.println("GetConditionName " + result);
		return result;
	}

	/** allows comparison where both terms being null evaluates as equality */
	public static boolean bothNullOrEqual(Object object1, Object object2) {
		boolean result = false;
		if ((object1 == null && object2 == null) || (object1 != null && object2 != null && object1.equals(object2))) {
			result = true;
		}
		return result;
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
	 * Returns whether the user has access to the specified module
	 *
	 * @param moduleName
	 * @return
	 */
	public static boolean hasModule(String moduleName) {
		boolean result = false;
		User user = CORE.getPersistence().getUser();
		Customer customer = user.getCustomer();
		for (Module module : customer.getModules()) {
			if (module.getName().equals(moduleName)) {
				result = true;
				break;
			}
		}
		return result;
	}

	/** short-hand way of finding a bean using a legacy key */
	public static Bean lookupBean(String moduleName, String documentName, String propertyName, Object objValue) {
		Persistence persistence = CORE.getPersistence();
		DocumentQuery qBean = persistence.newDocumentQuery(moduleName, documentName);
		qBean.getFilter().addEquals(propertyName, objValue);
		List<Bean> beans = qBean.beanResults();
		Bean bean = null;
		if (!beans.isEmpty()) {
			bean = beans.get(0);
		} else {
			System.out.println("Cannot find reference to " + objValue + " in document " + documentName);
		}

		return bean;
	}

	/**
	 * Returns the persistent table name for the specified module and document.
	 */
	public static String getPersistentIdentifier(final String moduleName, final String documentName) {
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		Persistent p = document.getPersistent();
		return (p == null) ? null : p.getPersistentIdentifier();
	}

	/**
	 * Convenience method for returning autocomplete suggestions for a String attribute based on previous values
	 *
	 * @param moduleName
	 * @param documentName
	 * @param attributeName
	 * @param value
	 * @return
	 * @throws Exception
	 */
	public static List<String> getCompleteSuggestions(String moduleName, String documentName, String attributeName, String value)
			throws Exception {
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(moduleName, documentName);
		if (value != null) {
			q.getFilter().addLike(attributeName, value + "%");
		}
		q.addBoundProjection(attributeName, attributeName);
		q.addBoundOrdering(attributeName);
		q.setDistinct(true);
		return q.scalarResults(String.class);
	}

	/**
	 * Configure a permissions group with at least the roleNames specified
	 * 
	 * @param name
	 * @param roleNames
	 */
	public static GroupExtension configureGroup(String name, String... roleNames) {
		// Configure required Staff permissions group
		DocumentQuery qGroup = CORE.getPersistence().newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		qGroup.getFilter().addEquals(Group.namePropertyName, name);
		boolean saveRequired = false;
		GroupExtension g = qGroup.beanResult();
		if (g == null) {
			g = Group.newInstance();
			g.setName(name);
			saveRequired = true;
		}
		// check roles
		for (String s : roleNames) {
			boolean found = false;
			for (GroupRole gr : g.getRoles()) {
				if (s.equals(gr.getRoleName())) {
					found = true;
					break;
				}
			}
			if (!found) {
				GroupRole gr = GroupRole.newInstance();
				gr.setRoleName(s);
				g.addRolesElement(gr);
				saveRequired = true;
			}
		}

		// and Save the group
		if (saveRequired) {
			g = CORE.getPersistence().save(g);
		}

		return g;
	}

}
