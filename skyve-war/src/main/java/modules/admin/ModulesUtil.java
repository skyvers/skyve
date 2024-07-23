package modules.admin;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal10;
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
import org.skyve.util.CommunicationUtil;
import org.skyve.util.Time;

import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import modules.admin.UserList.UserListUtil;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;
import modules.admin.domain.UserProxy;

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

	/** general types of time-based frequencies */
	public static enum OccurenceFrequency {
		OneOff, EverySecond, EveryMinute, Hourly, Daily, Weekly, Fortnightly, Monthly, Quarterly, HalfYearly, Yearly, Irregularly, DuringHolidays, NotDuringHolidays, WeekDays, Weekends;
	}

	public static final List<DomainValue> OCCURRENCE_FREQUENCIES = new ArrayList<>();

	static {
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Irregularly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.OneOff.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Hourly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Daily.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Weekly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Fortnightly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Monthly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Quarterly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.HalfYearly.toString()));
		OCCURRENCE_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Yearly.toString()));
	}

	/** subset of frequencies relevant for use as terms */
	public static final List<DomainValue> TERM_FREQUENCIES = new ArrayList<>();

	static {
		TERM_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Irregularly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Weekly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Fortnightly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Monthly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Quarterly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurenceFrequency.HalfYearly.toString()));
		TERM_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Yearly.toString()));
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
	 * Returns the number of periods of specified frequency which occur in the
	 * calendar year.
	 *
	 * @param frequency
	 *        - the specified frequency (OccurrenceFrequency)
	 * @return - the number of times the specified frequency occurs in a
	 *         calendar year
	 */
	public static int annualFrequencyCount(OccurenceFrequency frequency) {
		int periodCount = 1; // default period Count

		if (frequency.equals(OccurenceFrequency.Daily)) {
			// estimated
			periodCount = 365;
		} else if (frequency.equals(OccurenceFrequency.Weekly)) {
			periodCount = 52;
		} else if (frequency.equals(OccurenceFrequency.Fortnightly)) {
			periodCount = 26;
		} else if (frequency.equals(OccurenceFrequency.Monthly)) {

			periodCount = 12;
		} else if (frequency.equals(OccurenceFrequency.Quarterly)) {
			periodCount = 4;
		} else if (frequency.equals(OccurenceFrequency.HalfYearly)) {
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
	public static final DateOnly addFrequency(OccurenceFrequency frequency, DateOnly date, int numberOfFrequencies) {
		if (date != null) {
			if (frequency.equals(OccurenceFrequency.OneOff)) {
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

			if (frequency.equals(OccurenceFrequency.Daily)) {
				calendar.add(Calendar.DATE, numberOfFrequencies);
			} else if (frequency.equals(OccurenceFrequency.Weekly)) {
				calendar.add(Calendar.DATE, (numberOfFrequencies * 7));
			} else if (frequency.equals(OccurenceFrequency.Fortnightly)) {
				calendar.add(Calendar.DATE, (numberOfFrequencies * 14));
			} else if (frequency.equals(OccurenceFrequency.Monthly)) {
				calendar.add(Calendar.MONTH, numberOfFrequencies);
			} else if (frequency.equals(OccurenceFrequency.Quarterly)) {
				calendar.add(Calendar.MONTH, (numberOfFrequencies * 3));
			} else if (frequency.equals(OccurenceFrequency.HalfYearly)) {
				calendar.add(Calendar.MONTH, (numberOfFrequencies * 6));
			} else if (frequency.equals(OccurenceFrequency.Yearly)) {
				calendar.add(Calendar.YEAR, numberOfFrequencies);
			}

			newDate.setTime(calendar.getTime().getTime());

			return newDate;
		}
		return null;
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

	/** abbreviated forms of calendar months */
	public static enum CalendarMonth {
		JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
	}

	public static final List<DomainValue> CALENDAR_MONTHS = new ArrayList<>();

	static {
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.JAN.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.FEB.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.MAR.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.APR.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.MAY.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.JUN.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.JUL.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.AUG.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.SEP.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.OCT.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.NOV.toString()));
		CALENDAR_MONTHS.add(new DomainValue(CalendarMonth.DEC.toString()));
	}

	/** conversion from month Name to calendar month (int) */
	public static int calendarMonthNameToNumber(String monthName) {
		if (CalendarMonth.JAN.toString().equals(monthName)) {
			return 0;
		} else if (CalendarMonth.FEB.toString().equals(monthName)) {
			return 1;
		} else if (CalendarMonth.MAR.toString().equals(monthName)) {
			return 2;
		} else if (CalendarMonth.APR.toString().equals(monthName)) {
			return 3;
		} else if (CalendarMonth.MAY.toString().equals(monthName)) {
			return 4;
		} else if (CalendarMonth.JUN.toString().equals(monthName)) {
			return 5;
		} else if (CalendarMonth.JUL.toString().equals(monthName)) {
			return 6;
		} else if (CalendarMonth.AUG.toString().equals(monthName)) {
			return 7;
		} else if (CalendarMonth.SEP.toString().equals(monthName)) {
			return 8;
		} else if (CalendarMonth.OCT.toString().equals(monthName)) {
			return 9;
		} else if (CalendarMonth.NOV.toString().equals(monthName)) {
			return 10;
		} else if (CalendarMonth.DEC.toString().equals(monthName)) {
			return 11;
		} else {
			return 0;
		}
	}

	/**
	 * Returns the current session/conversation user as an Admin module User
	 *
	 * @return The current {@link modules.admin.User.UserExtension}
	 */
	public static UserExtension currentAdminUser() {
		UserExtension result = null;
		try {
			Persistence p = CORE.getPersistence();
			result = p.retrieve(modules.admin.domain.User.MODULE_NAME,
									modules.admin.domain.User.DOCUMENT_NAME,
									p.getUser().getId());
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing
		}

		return result;
	}

	/**
	 * Creates a new admin User for a given contact
	 * - sets the new user name to be the contact email address
	 * - adds the specified group privileges to the user
	 * - sets their home Module (if provided)
	 * - sets an expired password (to force them to reset their password)
	 * - sets a password reset token that can be provided to the user to reset their password
	 * - optionally sends an invitation email
	 *
	 * @param contact the Contact to create the new User from
	 * @param groupName The name of the group
	 * @param homeModuleName
	 * @param sendInvitation
	 * @return
	 */
	public static UserExtension createAdminUserFromContactWithGroup(Contact contact, final String groupName,
			final String homeModuleName, final boolean sendInvitation) {


		if (contact == null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.contact");
		}

		if (groupName == null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.groupName");
		}

		// check if user already exists
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(modules.admin.domain.User.MODULE_NAME, modules.admin.domain.User.DOCUMENT_NAME);
		q.getFilter().addEquals(modules.admin.domain.User.userNamePropertyName, contact.getEmail1());
		q.setMaxResults(1);

		UserExtension found = q.beanResult();
		if (found != null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.duplicateUser");
		}

		// check the group exists
		DocumentQuery qGroup = CORE.getPersistence().newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		qGroup.getFilter().addEquals(Group.namePropertyName, groupName);
		qGroup.setMaxResults(1);
		GroupExtension group = qGroup.beanResult();

		if (group == null) {
			throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.invalidGroup");
		}

		// check the home module name exists (Skyve will throw if it doesn't)
		CORE.getCustomer().getModule(homeModuleName);

		// save the contact to validate the contact and so that it can be referenced by the user
		Contact newContact = CORE.getPersistence().save(contact);

		final String token = UUID.randomUUID().toString() + Long.toString(System.currentTimeMillis());
		// create a user - not with a generated password
		UserExtension newUser = modules.admin.domain.User.newInstance();
		newUser.setUserName(newContact.getEmail1());
		newUser.setPassword(EXT.hashPassword(token));
		newUser.setPasswordExpired(Boolean.TRUE);
		newUser.setPasswordResetToken(token);
		newUser.setHomeModule(homeModuleName);
		newUser.setContact(newContact);

		// assign group
		newUser.getGroups().add(group);

		newUser = CORE.getPersistence().save(newUser);

		if (sendInvitation) {
			try {
				// send invitation email
				CommunicationUtil.sendFailSafeSystemCommunication(UserListUtil.SYSTEM_USER_INVITATION,
																  UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_SUBJECT,
																  UserListUtil.SYSTEM_USER_INVITATION_DEFAULT_BODY,
																  CommunicationUtil.ResponseMode.EXPLICIT, null, newUser);

			} catch (Exception e) {
				throw new DomainException("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.invitation", e);
			}
		}
		return newUser;
	}

	/**
	 * Returns the current session/conversation user as an Admin module UserProxy
	 *
	 * @return The current {@link modules.admin.domain.UserProxy}
	 */
	public static UserProxyExtension currentAdminUserProxy() {
		UserProxyExtension result = null;
		try {
			Persistence p = CORE.getPersistence();
			result = p.retrieve(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, p.getUser().getId());
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing
		}

		return result;
	}

	public static Contact getCurrentUserContact() {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Contact.MODULE_NAME);
		Document document = module.getDocument(customer, Contact.DOCUMENT_NAME);

		Contact contact = persistence.retrieve(document, user.getContactId());

		return contact;
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
	 * @param incrementingNumber	The number to generate a unique pseudo random number for
	 * @return	The quadratic residue.
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

	/** type-specific coalesce */
	public static String coalesce(Object val, String ifNullValue) {
		return (val == null ? ifNullValue : val.toString());
	}

	/** type-specific coalesce */
	public static String coalesce(String val, String ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** type-specific coalesce */
	public static Boolean coalesce(Boolean val, Boolean ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** type-specific coalesce */
	public static Integer coalesce(Integer val, Integer ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** type-specific coalesce */
	public static Decimal2 coalesce(Decimal2 val, Decimal2 ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** returns null if zero - for reports or data import/export */
	public static Decimal5 coalesce(Decimal5 val, Decimal5 ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** returns null if zero - for reports or data import/export */
	public static Decimal10 coalesce(Decimal10 val, Decimal10 ifNullValue) {
		return (val == null ? ifNullValue : val);
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
					term = coalesce(value, "").toString();
				}

			}

			// move along
			String displayValue = ModulesUtil.coalesce(term, "");
			result.replace(openCurlyBraceIndex, closedCurlyBraceIndex + 1, displayValue);
			openCurlyBraceIndex = result.indexOf("{");
		}

		return result.toString();
	}

	/** simple concatenation with a delimiter */
	public static String concatWithDelim(String delimiter, String... strings) {
		StringBuilder sb = new StringBuilder();
		String delim = coalesce(delimiter, " ");

		for (String s : strings) {
			if (coalesce(s, "").length() > 0) {
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
			if (coalesce(quoteSet, "").length() > 1) {

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
	public static List<String> getCompleteSuggestions(String moduleName, String documentName, String attributeName, String value) throws Exception {
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
