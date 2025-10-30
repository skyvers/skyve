package modules.admin;

import java.util.Calendar;
import java.util.Comparator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Coalesce;
import org.skyve.util.MonthDomainValues;
import org.skyve.util.ScheduleUtil;
import org.skyve.util.Time;

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
	public static DateOnly lastDayOfMonth(DateOnly date) {
		if (date != null) {
			DateOnly newDate = new DateOnly(date.getTime());
			Calendar calendar = Calendar.getInstance();
			calendar.setTime(newDate);
			calendar.setLenient(false);

			// NB clear() does not work in JDK 1.3.1
			calendar.set(Calendar.MILLISECOND, 0);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MINUTE, 0);
			calendar.set(Calendar.HOUR_OF_DAY, 0);

			// last day of month is one day before 1st day of next month
			calendar.add(Calendar.MONTH, 1);
			calendar.set(Calendar.DATE, 1);

			newDate.setTime(calendar.getTime().getTime());
			Time.addDays(newDate, -1);

			return newDate;
		}
		return null;
	}

	/**
	 * Returns the last day of the year in which the specified date occurs.
	 *
	 * @param date
	 *        - the specified date
	 * @return - the date of the last day of the year in which the specified
	 *         date occurs
	 */
	public static DateOnly lastDayOfYear(DateOnly date) {
		if (date != null) {
			DateOnly newDate = new DateOnly(date.getTime());
			Calendar calendar = Calendar.getInstance();
			calendar.setTime(newDate);
			calendar.setLenient(false);

			// NB clear() does not work in JDK 1.3.1
			calendar.set(Calendar.MILLISECOND, 0);
			calendar.set(Calendar.SECOND, 0);
			calendar.set(Calendar.MINUTE, 0);
			calendar.set(Calendar.HOUR_OF_DAY, 0);

			// last day of year is one day before 1st day of next year
			calendar.add(Calendar.YEAR, 1);
			calendar.set(Calendar.MONTH, 0);
			calendar.set(Calendar.DATE, 1);

			newDate.setTime(calendar.getTime().getTime());
			Time.addDays(newDate, -1);

			return newDate;
		}
		return null;
	}

	/**
	 * Returns the date of the first day of the month in which the specified
	 * date occurs.
	 *
	 * @param date
	 *        - the specified date
	 * @return - the date of the first day of that month
	 */
	@SuppressWarnings("deprecation")
	public static DateOnly firstDayOfMonth(DateOnly date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.setLenient(false);

		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		calendar.set(Calendar.MONTH, date.getMonth());
		calendar.set(Calendar.DATE, 1);

		date.setTime(calendar.getTime().getTime());

		return date;
	}

	/**
	 * Returns the date of the first day of the year in which the specified date
	 * occurs.
	 *
	 * @param date
	 *        - the specified date
	 * @return - the date of the first day of that year
	 */
	public static DateOnly firstDayOfYear(DateOnly date) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.setLenient(false);

		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		calendar.set(Calendar.MONTH, 0);
		calendar.set(Calendar.DATE, 1);

		date.setTime(calendar.getTime().getTime());

		return date;
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
	public static DateOnly addDaysDateOnly(DateOnly date, int daysToAdd) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.setLenient(false);

		// NB clear() does not work in JDK 1.3.1
		calendar.set(Calendar.MILLISECOND, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.HOUR_OF_DAY, 0);

		calendar.add(Calendar.DAY_OF_WEEK, daysToAdd);

		date.setTime(calendar.getTime().getTime());

		return date;
	}

	@SuppressWarnings("deprecation")
	public static String sqlFormatDateOnly(DateOnly theDate) {
		String result = "";

		if (theDate != null) {
			String month = "0" + (theDate.getMonth() + 1);
			month = month.substring(month.length() - 2);
			String day = "0" + theDate.getDate();
			day = day.substring(day.length() - 2);

			result = "convert('" + (theDate.getYear() + 1900) + "-" + month + "-" + day + "', date) ";
		}

		return result;
	}

	/** Returns a TitleCase version of the String supplied */
	public static String titleCase(String raw) {
		String s = raw;
		if (s != null) {
			if (s.length() > 1) {
				s = s.substring(0, 1).toUpperCase() + s.substring(1);
			} else if (s.length() == 1) {
				s = s.toUpperCase();
			}
		}

		return s;
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
	public static String replaceBindingsInString(Bean bean, String replacementString) throws Exception {

		StringBuilder result = new StringBuilder(replacementString);
		int openCurlyBraceIndex = result.indexOf("{");

		// now replace contents of each curlyBraced expression if we can
		while (openCurlyBraceIndex >= 0) {
			int closedCurlyBraceIndex = result.indexOf("}");
			String displayNameOfAttribute = result.substring(openCurlyBraceIndex + 1, closedCurlyBraceIndex);

			Bean b = bean;
			StringBuilder binding = new StringBuilder();
			String[] attributes = displayNameOfAttribute.toString().split("\\.");
			boolean found = false;
			for (String a : attributes) {

				// if the format string includes a sub-bean attribute, get the
				// sub-bean
				if (binding.toString().length() > 0) {
					b = (Bean) Binder.get(bean, binding.toString());
				}

				// parent special case
				if (a.equals("parent")) {
					b = ((ChildBean<?>) bean).getParent();
				}

				// if the sub-bean isn't null, try to match the attribute
				// because attributes in the format string might be optional
				found = false;
				if (b != null) {

					User user = CORE.getPersistence().getUser();
					Customer customer = user.getCustomer();
					Module module = customer.getModule(b.getBizModule());
					Document document = module.getDocument(customer, b.getBizDocument());

					for (Attribute attribute : document.getAllAttributes(customer)) {
						if (attribute.getLocalisedDisplayName().equals(a)) {
							found = true;
							if (binding.toString().length() > 0) {
								binding.append('.').append(attribute.getName());
							} else {
								binding.append(attribute.getName());

							}
						}
					}

					// check for non-attribute bindings
					if (!found) {
						try {
							if (Binder.get(bean, a) != null) {
								binding.append(a);
							}
						} catch (@SuppressWarnings("unused") Exception e) {
							// do nothing
						}
					}
				}
			}

			String term = "";
			if (found) {
				Object value = Binder.get(bean, binding.toString());

				if (value instanceof DateOnly) {
					DateOnly dValue = (DateOnly) value;
					DD_MMM_YYYY convDate = new DD_MMM_YYYY();

					term = convDate.toDisplayValue(dValue);
				} else if (value instanceof Decimal2) {
					term = value.toString();
				} else if (value instanceof Decimal5) {
					term = value.toString();
				} else {
					term = Coalesce.coalesceNull(value, "").toString();
				}

			}

			// move along
			String displayValue = Coalesce.coalesceNull(term, "");
			result.replace(openCurlyBraceIndex, closedCurlyBraceIndex + 1, displayValue);
			openCurlyBraceIndex = result.indexOf("{");
		}

		return result.toString();
	}

	/** simple concatenation with a delimiter */
	public static String concatWithDelim(String delimiter, String... strings) {
		StringBuilder sb = new StringBuilder();
		String delim = Coalesce.coalesceNull(delimiter, " ");

		for (String s : strings) {
			if (Coalesce.coalesceNull(s, "").length() > 0) {
				if (sb.toString().length() > 0) {
					sb.append(delim);
				}
				sb.append(s);
			}
		}

		return sb.toString();
	}

	/** short-hand enquoting of a string */
	public static String enquote(String quoteSet, String s) {
		String l = null;
		String r = null;
		if (quoteSet != null) {

			l = quoteSet.substring(0, 1);
			if (Coalesce.coalesceNull(quoteSet, "").length() > 1) {

				r = quoteSet.substring(1);
			}
		}
		return concatWithDelim("", l, concatWithDelim("", s, r));
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
