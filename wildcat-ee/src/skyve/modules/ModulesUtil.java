package modules;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Formatter;
import java.util.GregorianCalendar;
import java.util.List;

import modules.admin.domain.Contact;
import modules.admin.domain.DocumentNumber;

import org.skyve.wildcat.bizport.StandardGenerator;
import org.skyve.wildcat.bizport.StandardLoader;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.bizport.BizPortException;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.ValidationMessage;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Time;

/**
 * Utility methods applicable across application modules.
 * <p>
 * This class is provided as part of WILDCAT
 * 
 * @author robert.brown
 * 
 */
public class ModulesUtil {

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
			return d1.getDescription().compareTo(d2.getDescription());
		}
	}

	/** the rate of Goods and Services Tax applicable in Australia */
	public static final Decimal2 GST_RATE = new Decimal2(0.1);

	/** general status types for application documents */
	public static enum DocumentStatus {
		DRAFT, SUBMITTED, AUTHORISED, REJECTED, LOCKED, UNLOCKED, PAID, LODGED
	}

	public static final List<DomainValue> DOCUMENT_STATES = new ArrayList<>();

	static {
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.DRAFT.toString()));
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.SUBMITTED.toString()));
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.AUTHORISED.toString()));
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.REJECTED.toString()));
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.LOCKED.toString()));
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.UNLOCKED.toString()));
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.PAID.toString()));
		DOCUMENT_STATES.add(new DomainValue(DocumentStatus.LODGED.toString()));
	}

	public static final List<DomainValue> EMPLOYEE_PAYMENT_TYPES = new ArrayList<>();

	static {
		EMPLOYEE_PAYMENT_TYPES.add(new DomainValue("Salary"));
		EMPLOYEE_PAYMENT_TYPES.add(new DomainValue("Wages"));
	}

	/** special type of payment considered to be tax */
	public static final String TAX_PAY_SHEET_PAYMENT_TYPE = "Tax";

	/** special type of payment considered to be superannuation */
	public static final String SUPER_PAY_SHEET_PAYMENT_TYPE = "Superannuation";

	/** general status of payments */
	public static enum PaymentStatus {
		UNPAID, PENDING, PAID, WITHHELD
	}

	public static final List<DomainValue> PAYMENT_STATUS_TYPES = new ArrayList<>();

	static {
		PAYMENT_STATUS_TYPES.add(new DomainValue(PaymentStatus.UNPAID.toString()));
		PAYMENT_STATUS_TYPES.add(new DomainValue(PaymentStatus.PENDING.toString()));
		PAYMENT_STATUS_TYPES.add(new DomainValue(PaymentStatus.PAID.toString()));
		PAYMENT_STATUS_TYPES.add(new DomainValue(PaymentStatus.WITHHELD.toString()));
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

	/** subset of time-based frequencies relevant for paysheets */
	public static final List<DomainValue> PAY_SHEET_PAYMENT_FREQUENCIES = new ArrayList<>();

	static {
		PAY_SHEET_PAYMENT_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Irregularly.toString()));
		PAY_SHEET_PAYMENT_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Weekly.toString()));
		PAY_SHEET_PAYMENT_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Fortnightly.toString()));
		PAY_SHEET_PAYMENT_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Monthly.toString()));
		PAY_SHEET_PAYMENT_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Quarterly.toString()));
		PAY_SHEET_PAYMENT_FREQUENCIES.add(new DomainValue(OccurenceFrequency.HalfYearly.toString()));
		PAY_SHEET_PAYMENT_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Yearly.toString()));
	}

	/** subset of time-based frequencies relevant for invoices */
	public static final List<DomainValue> INVOICING_FREQUENCIES = new ArrayList<>();

	static {
		INVOICING_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Irregularly.toString()));
		INVOICING_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Weekly.toString()));
		INVOICING_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Fortnightly.toString()));
		INVOICING_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Monthly.toString()));
		INVOICING_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Quarterly.toString()));
		INVOICING_FREQUENCIES.add(new DomainValue(OccurenceFrequency.HalfYearly.toString()));
		INVOICING_FREQUENCIES.add(new DomainValue(OccurenceFrequency.Yearly.toString()));
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

	/** general priority levels */
	public static enum PriorityType {
		High, Medium, Low
	}

	public static final List<DomainValue> PRIORITY_TYPES = new ArrayList<>();

	static {
		PRIORITY_TYPES.add(new DomainValue(PriorityType.High.toString()));
		PRIORITY_TYPES.add(new DomainValue(PriorityType.Medium.toString()));
		PRIORITY_TYPES.add(new DomainValue(PriorityType.Low.toString()));
	}

	/** general specifier for accounting records */
	public static enum CreditType {
		Credit, Debit
	}

	public static final List<DomainValue> CREDIT_TYPES = new ArrayList<>();

	static {
		CREDIT_TYPES.add(new DomainValue(CreditType.Credit.toString()));
		CREDIT_TYPES.add(new DomainValue(CreditType.Debit.toString()));
	}

	/**
	 * position of payment within a period for the purposes of calculating
	 * interest payments
	 */
	public static enum PaymentPosition {
		StartOfPeriod, EndOfPeriod
	}

	public static final List<DomainValue> PAYMENT_POSITIONS = new ArrayList<>();

	static {
		PAYMENT_POSITIONS.add(new DomainValue(PaymentPosition.StartOfPeriod.toString()));
		PAYMENT_POSITIONS.add(new DomainValue(PaymentPosition.EndOfPeriod.toString()));
	}

	/** generally accepted methods of payment */
	public static enum PaymentMethod {
		Cash, Cheque, EFT, Card, BPay
	}

	public static final List<DomainValue> PAYMENT_METHODS = new ArrayList<>();

	static {
		PAYMENT_METHODS.add(new DomainValue(PaymentMethod.Cash.toString()));
		PAYMENT_METHODS.add(new DomainValue(PaymentMethod.Cheque.toString()));
		PAYMENT_METHODS.add(new DomainValue(PaymentMethod.EFT.toString()));
		PAYMENT_METHODS.add(new DomainValue(PaymentMethod.Card.toString()));
		PAYMENT_METHODS.add(new DomainValue(PaymentMethod.BPay.toString()));
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
	 *            - the day of the week (DayOfWeek)
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
	 *            - the number of the day (int)
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
	 *            - the specified frequency (OccurrenceFrequency)
	 * @return - the number of times the specified frequency occurs in a
	 *         calendar year
	 */
	public static int annualFrequencyCount(OccurenceFrequency frequency) {
		int periodCount = 1; // default period Count

		if (frequency.equals(OccurenceFrequency.Daily)) {
			// need to work out whether this year is a leap year
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
	 *            - the time period (OccurrencePeriod)
	 * @return - the number of times the period occurs within a calendar year
	 *         (int)
	 */
	public static int annualPeriodCount(OccurrencePeriod period) {
		int periodCount = 1; // default period Count

		if (period.equals(OccurrencePeriod.Days)) {
			// need to work out whether this year is a leap year
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
	 *            - the frequency to add
	 * @param date
	 *            - the date to add to
	 * @param numberOfFrequencies
	 *            - the number of frequencies to add
	 * @return - the resulting date
	 */
	public static final DateOnly addFrequency(OccurenceFrequency frequency, DateOnly date, int numberOfFrequencies) {
		if (date != null) {
			if (frequency.equals(OccurenceFrequency.OneOff)) {
				return new DateOnly(date.getTime());
			}

			DateOnly newDate = new DateOnly(date.getTime());
			Calendar calendar = new GregorianCalendar();
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
	 *            - the specified date
	 * @return - the date of the last day of the month in which the specified
	 *         date occurs
	 */
	public static DateOnly lastDayOfMonth(DateOnly date) {
		if (date != null) {
			DateOnly newDate = new DateOnly(date.getTime());
			Calendar calendar = new GregorianCalendar();
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
	 *            - the specified date
	 * @return - the date of the last day of the year in which the specified
	 *         date occurs
	 */
	public static DateOnly lastDayOfYear(DateOnly date) {
		if (date != null) {
			DateOnly newDate = new DateOnly(date.getTime());
			Calendar calendar = new GregorianCalendar();
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
	 * general types of whether something is considered to be an asset or
	 * liability for accounting purposes
	 */
	public static enum AssetType {
		Asset, Liability;
	}

	public static final List<DomainValue> ASSET_TYPES = new ArrayList<>();

	static {
		ASSET_TYPES.add(new DomainValue(AssetType.Asset.toString()));
		ASSET_TYPES.add(new DomainValue(AssetType.Liability.toString()));
	}

	/**
	 * assumed multiplier for asset type based on the assumption that assets are
	 * positive value
	 */
	public static Decimal2 assetTypeMultiplier(AssetType assetType) {
		if (assetType.equals(AssetType.Liability)) {
			return new Decimal2(-1);
		}
		return new Decimal2(1);
	}

	/**
	 * general type specifying whether the item has residual value for
	 * accounting purposes
	 */
	public static enum ConsumableType {
		// Asset is used here instead of "non-consumable"
		Consumable, Asset
	}

	public static final List<DomainValue> CONSUMABLE_TYPES = new ArrayList<>();

	static {
		CONSUMABLE_TYPES.add(new DomainValue(ConsumableType.Consumable.toString()));
		CONSUMABLE_TYPES.add(new DomainValue(ConsumableType.Asset.toString()));
	}

	/** general types of income for accounting purposes */
	public static enum IncomeType {
		// Transfer and Repayment are synonymous but included for clarity for
		// the user
		Income, Expense, Transfer, Repayment, Drawings, CapitalContribution, Wages, Superannuation, Interest, OtherReceipts, Fees;
	}

	public static final List<DomainValue> INCOME_TYPES = new ArrayList<>();

	static {
		INCOME_TYPES.add(new DomainValue(IncomeType.Income.toString()));
		INCOME_TYPES.add(new DomainValue(IncomeType.Expense.toString()));
		INCOME_TYPES.add(new DomainValue(IncomeType.Transfer.toString()));
		INCOME_TYPES.add(new DomainValue(IncomeType.Repayment.toString()));
	}

	/**
	 * returns the opposite income types - used for effectively negating or
	 * reversing the transaction
	 */
	public static IncomeType oppositeIncomeType(String incomeType) {
		IncomeType opposite;
		if (IncomeType.Expense.toString().equals(incomeType)) {
			opposite = IncomeType.Income;

		} else if (IncomeType.Income.toString().equals(incomeType)) {
			opposite = IncomeType.Expense;
		} else {
			opposite = IncomeType.valueOf(incomeType);
		}

		return opposite;
	}

	/**
	 * multiplier relevant assuming that income represents positive value for
	 * the purposes of accounting
	 */
	public static Decimal2 incomeTypeMultiplier(IncomeType incomeType) {
		if (incomeType.equals(IncomeType.Expense)) {
			return new Decimal2(-1);
		}
		return new Decimal2(1);
	}

	/**
	 * income type assuming that income represents positive value for the
	 * purposes of accounting
	 */
	public static IncomeType incomeTypeFromDecimal2(Decimal2 value) {
		IncomeType incomeType;

		if (value.intValue() > 0) {
			incomeType = IncomeType.Income;
		} else {
			incomeType = IncomeType.Expense;
		}

		return incomeType;
	}

	/**
	 * classifier of whether the item is subject to the Goods and Services Tax
	 * in Australia for accounting purposes
	 */
	public static enum SubjectToGSTType {
		SubjectToGST, NotSubjectToGST;
	}

	public static final List<DomainValue> SUBJECT_TO_GST_TYPES = new ArrayList<>();

	static {
		SUBJECT_TO_GST_TYPES.add(new DomainValue(SubjectToGSTType.SubjectToGST.toString()));
		SUBJECT_TO_GST_TYPES.add(new DomainValue(SubjectToGSTType.NotSubjectToGST.toString()));
	}

	/**
	 * whether the asset is considered to be increasing (appreciating) or
	 * decreasing in value
	 */
	public static enum AppreciationType {
		Appreciate, Depreciate;
	}

	public static final List<DomainValue> APPRECIATION_TYPES = new ArrayList<>();

	static {
		APPRECIATION_TYPES.add(new DomainValue(AppreciationType.Appreciate.toString()));
		APPRECIATION_TYPES.add(new DomainValue(AppreciationType.Depreciate.toString()));
	}

	/**
	 * created in anticipation of future legislative changes to the Goods and
	 * Services Tax in Australia
	 */
	public static Decimal2 gstRate() {
		// if(applicableDateOnly.after(new DateOnly()))
		return new Decimal2(0.1);
	}

	/**
	 * calculates the total of tax applicable under the Goods and Services Tax
	 * in Australia
	 */
	public static Decimal2 valueOfGST(Decimal2 amount, boolean isExGST) {
		if (isExGST) {
			return amount.multiply(gstRate());
		}
		return amount.divide(Decimal2.ONE.add(gstRate()));
	}

	/**
	 * calculates the total excluding tax applicable under the Goods and
	 * Services Tax in Australia
	 */
	public static Decimal2 exGSTAmount(Decimal2 incGSTAmount) {
		return incGSTAmount.multiply(Decimal2.ONE.subtract(gstRate()));
	}

	/**
	 * calculates the total including tax applicable under the Goods and
	 * Services Tax in Australia
	 */
	public static Decimal2 incGSTAmount(Decimal2 exGSTAmount) {
		return exGSTAmount.add(valueOfGST(exGSTAmount, true));
	}

	/**
	 * Returns the date of the first day of the month in which the specified
	 * date occurs.
	 * 
	 * @param date
	 *            - the specified date
	 * @return - the date of the first day of that month
	 */
	@SuppressWarnings("deprecation")
	public static DateOnly firstDayOfMonth(DateOnly date) {
		Calendar calendar = new GregorianCalendar();
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
	 *            - the specified date
	 * @return - the date of the first day of that year
	 */
	public static DateOnly firstDayOfYear(DateOnly date) {
		Calendar calendar = new GregorianCalendar();
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
	 *            - the specified date
	 * @param daysToAdd
	 *            - the number of days to add to that date
	 * @return - the resulting date
	 */
	public static DateOnly addDaysDateOnly(DateOnly date, int daysToAdd) {
		Calendar calendar = new GregorianCalendar();
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

	/** Returns the current session/conversation user as a Amin module User */
	public static modules.admin.domain.User currentAdminUser() {
		modules.admin.domain.User user = null;
		try {
			Persistence persistence = CORE.getPersistence();
			Customer customer = persistence.getUser().getCustomer();
			Module module = customer.getModule(modules.admin.domain.User.MODULE_NAME);
			Document userDoc = module.getDocument(customer, modules.admin.domain.User.DOCUMENT_NAME);
			user = persistence.retrieve(userDoc, persistence.getUser().getId(), false);
		} catch (Exception e) {
			// do nothing
		}

		return user;
	}

	public static Contact getCurrentUserContact() throws MetaDataException, DomainException {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Contact.MODULE_NAME);
		Document document = module.getDocument(customer, Contact.DOCUMENT_NAME);

		Contact contact = persistence.retrieve(document, user.getContactId(), false);

		return contact;
	}
	
	public static void addValidationError(ValidationException e, String fieldName, String messageString) {
		ValidationMessage vM = new ValidationMessage(messageString);
		vM.addBinding(fieldName);
		ValidationException vE = new ValidationException(vM);
		e.getSubordinates().add(vE);
	}

	/**
	 * Returns a new document/sequence number for the given
	 * module.document.fieldName in a thread-safe way.
	 * <p>
	 * If no previous record is found in the DocumentNumber table, the method
	 * attempts to find the Maximum existing value currently extant in the field
	 * and increments that. Otherwise, the value returned is incremented and
	 * updated DocumentNumber value for the specified combination.
	 * 
	 * @param prefix
	 *            - if the sequence value has a known prefix before the number,
	 *            eg INV0001 has a prefix of "INV"
	 * @param moduleName
	 *            - the application module
	 * @param documentName
	 *            - the application document
	 * @param fieldName
	 *            - the fieldName/columnName in which the value is held
	 * @param numberLength
	 *            - the minimum length of the number when specified as a string
	 * @return - the next sequence number
	 * @throws Exception
	 *             general Exception for persistence failure
	 */
	public static String getNextDocumentNumber(String prefix, String moduleName, String documentName, String fieldName, int numberLength) throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(DocumentNumber.MODULE_NAME);
		Document document = module.getDocument(customer, DocumentNumber.DOCUMENT_NAME);

		String nextNumber = "0";
		String lastNumber = "0";

		DocumentNumber dN = null;
		try {
			DocumentQuery qN = pers.newDocumentQuery(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME);
			qN.getFilter().addEquals(DocumentNumber.moduleNamePropertyName, moduleName);
			qN.getFilter().addEquals(DocumentNumber.documentNamePropertyName, documentName);
			qN.getFilter().addEquals(DocumentNumber.sequenceNamePropertyName, fieldName);

			List<DocumentNumber> num = pers.retrieve(qN);
			if (num.isEmpty()) {

				// System.out.println("DOCUMENT NUMBER: No previous found - source from table");

				// Check if sequence name is a field in that table
				boolean isField = false;
				for (Attribute attribute : document.getAttributes()) {
					if (attribute.getName().equals(fieldName)) {
						isField = true;
						break;
					}
				}

				if (isField) {
					// first hit - go lookup max number from table
					DocumentQuery query = pers.newDocumentQuery(moduleName, documentName);
					query.addAggregateProjection(AggregateFunction.Max, fieldName, "MaxNumber");

					List<Bean> beans = pers.retrieve(query);
					if (!beans.isEmpty()) {
						Object o = Binder.get(beans.get(0), "MaxNumber");
						if (o instanceof Integer) {
							lastNumber = ((Integer) Binder.get(beans.get(0), "MaxNumber")).toString();
						} else {
							lastNumber = (String) Binder.get(beans.get(0), "MaxNumber");
						}
					}
				}

				// create a new document number record
				dN = document.newInstance(user);
				dN.setModuleName(moduleName);
				dN.setDocumentName(documentName);
				dN.setSequenceName(fieldName);

			} else {
				// System.out.println("DOCUMENT NUMBER: Previous found");
				dN = num.get(0);
				dN = pers.retrieve(document, dN.getBizId(), true); // issue a
																	// row-level
																	// lock
				lastNumber = dN.getNumber();
			}
			// just update from the document Number
			nextNumber = incrementAlpha(prefix, lastNumber, numberLength);
			dN.setNumber(nextNumber);

			pers.preFlush(document, dN);
			pers.upsertBeanTuple(dN);
			pers.postFlush(document, dN);
		} finally {
			if (dN != null) {
				pers.evictCached(dN);
			}
		}
		// System.out.println("Next document number for " + moduleName + "." +
		// documentName + "." + fieldName + " is " + nextNumber);

		return nextNumber;
	}

	/**
	 * Wrapper for getNextDocumentNumber, specifically for numeric only
	 * sequences
	 */
	public static Integer getNextDocumentNumber(String moduleName, String documentName, String fieldName) throws Exception {

		return new Integer(Integer.parseInt(getNextDocumentNumber(null, moduleName, documentName, fieldName, 0)));
	}

	/**
	 * Returns the next alpha value - ie A00A1 becomes A00A2 etc
	 * 
	 * @param prefix
	 *            - if the sequence value has a known prefix before the number,
	 *            eg INV0001 has a prefix of "INV"
	 * @param numberLength
	 *            - the minimum length of the number when specified as a string
	 * @param lastNumber
	 *            - the number to increment
	 * @return - the next number
	 * @throws Exception
	 *             general Exception
	 */
	public static String incrementAlpha(String prefix, String lastNumber, int numberLength) throws Exception {

		String newNumber = "";
		String nonNumeric = lastNumber;
		Integer value = new Integer(1);
		if (lastNumber != null) {
			String[] parts = (new StringBuilder(" ").append(lastNumber)).toString().split("\\D\\d+$");

			if (parts.length > 0 && parts[0].length() < lastNumber.length()) {
				String numberPart = lastNumber.substring(parts[0].length(), lastNumber.length());
				nonNumeric = lastNumber.substring(0, parts[0].length());

				value = new Integer(Integer.parseInt(numberPart) + 1);
			} else if (lastNumber.matches("^\\d+$")) {
				nonNumeric = (prefix == null ? "" : prefix);
				value = new Integer(Integer.parseInt(lastNumber) + 1);
			}
		} else {
			nonNumeric = (prefix == null ? "" : prefix);
		}

		// now put prefix and value together
		int newLength = (nonNumeric.length() + value.toString().length() > numberLength ? nonNumeric.length() + value.toString().length() : numberLength);

		StringBuilder sb = new StringBuilder(newLength + 1);
		try (Formatter f = new Formatter(sb)) {
			newNumber = nonNumeric + f.format(new StringBuilder("%1$").append(newLength - nonNumeric.length()).append("s").toString(), value.toString()).toString().replace(" ", "0");
		}
		
		return newNumber;
	}

	/** short-hand generic way to create a bean instance */
	public static Bean newBeanInstance(String moduleName, String documentName) throws Exception {
		User user = CORE.getPersistence().getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);

		Bean bean = document.newInstance(user);

		return bean;
	}

	/**
	 * Calculates minimum repayment for loan P = iA / (1-(1+i)-N
	 * 
	 * @param principle
	 *            - the principal amount of the loan
	 * @param annualRate
	 *            - the annual interest rate
	 * @param term
	 *            - the term of the loan
	 * @param termPeriod
	 *            - the period units of the term
	 * @param repaymentFrequency
	 *            - the frequency repayments are made
	 * @return - the minimum amount of repayment to ensure the loan is repaid
	 */
	public static Decimal2 minimumLoanRepayment(Decimal2 principle, Decimal5 annualRate, int term, OccurrencePeriod termPeriod, OccurenceFrequency repaymentFrequency) {
		Decimal2 minRepayment = Decimal2.ZERO;

		if (principle != null && annualRate != null) {
			int periodCount = annualPeriodCount(termPeriod);
			int annualRepaymentCount = annualFrequencyCount(repaymentFrequency);

			if (periodCount != 0 && annualRepaymentCount != 0) {
				int totalPeriods = (annualRepaymentCount * term) / periodCount;
				Decimal5 periodRate = annualRate.divide(new Decimal5(annualRepaymentCount));
				minRepayment = new Decimal2(periodRate.multiply(principle.abs())).divide((Decimal5.ONE.subtract((Decimal5.ONE.add(periodRate)).pow(0 - totalPeriods))));
			}
		}
		return minRepayment;
	}

	/** Wrapper for minimumLoanRepayment using annual interest rates */
	public static Decimal2 minimumLoanRepayment(Decimal2 principle, Decimal2 annualRatePC, int term, String termPeriodString, String repaymentFrequencyString) {
		Decimal5 annualRate = new Decimal5(annualRatePC.divide(new Decimal2(100)));
		OccurrencePeriod termPeriod = OccurrencePeriod.valueOf(termPeriodString);
		OccurenceFrequency repaymentFrequency = OccurenceFrequency.valueOf(repaymentFrequencyString);

		return minimumLoanRepayment(principle, annualRate, term, termPeriod, repaymentFrequency);
	}

	/** returns a fomatted string representing the condition */
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

	/** returns null if zero - for reports or data import/export */
	public static Decimal5 coalesce(Decimal5 val, Decimal5 ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** type-specific coalesce */
	public static String coalesce(String val, String ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** type-specific coalesce */
	public static Decimal2 coalesce(Decimal2 val, Decimal2 ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** type-specific coalesce */
	public static String coalesce(Object val, String ifNullValue) {
		return (val == null ? ifNullValue : val.toString());
	}

	/** type-specific coalesce */
	public static Boolean coalesce(Boolean val, Boolean ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/** type-specific coalesce */
	public static Integer coalesce(Integer val, Integer ifNullValue) {
		return (val == null ? ifNullValue : val);
	}

	/**
	 * Replaces the value found in the bean for the binding string provided,
	 * e.g. if the bean has a binding of contact.name, for which the
	 * displayNames of those bindings are Contact.FullName , then get the value
	 * of that binding from the bean provided.
	 * 
	 * @param bean
	 *            - the bean relevant for the binding
	 * @param replacementString
	 *            - the string representing the displayName form of the binding
	 * @return - the value from the bean
	 * @throws Exception
	 *             general Exception for metadata exception or string
	 *             manipulation failure etc
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

					for (Attribute attribute : document.getAttributes()) {
						if (attribute.getDisplayName().equals(a)) {
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
						} catch (Exception e) {
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
	 * Generic bizport export method.
	 * 
	 * @param moduleName
	 *            - the module to be exported
	 * @param documentName
	 *            - the document to be exported
	 * @param b
	 *            - the top-level bean to export
	 * @return - the reference to the Bizportable
	 * @throws Exception
	 *             general Exception
	 */
	public static BizPortWorkbook standardBeanBizExport(String modName, String docName, Bean b) throws Exception {

		String documentName = docName;
		String moduleName = modName;

		BizPortWorkbook result = EXT.newBizPortWorkbook(false);

		if (b != null) {
			moduleName = b.getBizModule();
			documentName = b.getBizDocument();
		}

		Persistence persistence = CORE.getPersistence();
		Customer customer = persistence.getUser().getCustomer();
		Module module = customer.getModule(moduleName);

		// Project
		Document document = module.getDocument(customer, documentName);
		StandardGenerator bgBean = EXT.newBizPortStandardGenerator(customer, document);
		bgBean.generateStructure(result);

		result.materialise();

		// System.out.println("BIZPORTING PROJECT " );
		DocumentQuery query = persistence.newDocumentQuery(moduleName, documentName);
		if (b != null) {
			// filter for this project if provided
			query.getFilter().addEquals(Bean.DOCUMENT_ID, b.getBizId());
		}
		bgBean.generateData(result, persistence.retrieve(query));

		return result;
	}

	public static void standardBeanBizImport(BizPortWorkbook workbook, BizPortException problems, String moduleName) throws Exception {
		final Persistence persistence = CORE.getPersistence();
		final Customer customer = persistence.getUser().getCustomer();
		StandardLoader loader = new StandardLoader(workbook, problems);
		List<Bean> bs = loader.populate(persistence);

		Module module = customer.getModule(moduleName);
		Document document = null;
		Bean bean = null;
		String documentName = null;

		// System.out.println("Validate everything " + new Date());
		for (String key : loader.getBeanKeys()) {
			bean = loader.getBean(key);
			document = module.getDocument(customer, bean.getBizDocument());
			if (documentName == null) {
				documentName = document.getName();
			}

			try {
				persistence.preFlush(document, bean);
			} catch (DomainException e) {
				loader.addError(customer, module, document, bean, e);
			}
		}

		// throw if we have errors found
		if (problems.hasErrors()) {
			throw problems;
		}

		// do the insert as 1 operation, bugging out if we encounter any errors
		try {
			document = module.getDocument(customer, documentName);
			for (Bean b : bs) {
				PersistentBean pb = (PersistentBean) b;
				pb = persistence.save(document, pb);
			}
		} catch (DomainException e) {
			loader.addError(customer, module, document, bean, e);
			throw problems;
		}
	}

	/** short-hand way of finding a bean using a legacy key */
	public static Bean lookupBean(String moduleName, String documentName, String propertyName, Object objValue) throws Exception {
		Persistence persistence = CORE.getPersistence();
		DocumentQuery qBean = persistence.newDocumentQuery(moduleName, documentName);
		qBean.getFilter().addEquals(propertyName, objValue);
		List<Bean> beans = persistence.retrieve(qBean);
		Bean bean = null;
		if (!beans.isEmpty()) {
			bean = beans.get(0);
		} else {
			System.out.println("Cannot find reference to " + objValue + " in document " + documentName);
		}

		return bean;
	}

	/**
	 * Returns whether the user has access to the specified module
	 * 
	 * @param moduleName
	 * @return
	 */
	public static boolean hasModule(String moduleName) throws MetaDataException {
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
}
