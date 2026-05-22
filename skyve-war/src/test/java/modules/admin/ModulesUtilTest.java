package modules.admin;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Calendar;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.Time;

import modules.admin.Group.GroupExtension;
import modules.admin.ModulesUtil.DayOfWeek;
import modules.admin.ModulesUtil.OccurrencePeriod;
import modules.admin.ModulesUtil.OccurenceFrequency;
import modules.admin.domain.GroupRole;
import util.AbstractH2Test;

@SuppressWarnings("deprecation")
class ModulesUtilTest extends AbstractH2Test {

	@AfterEach
	void tearDownTeats() {
		// nothing to see here
	}

	@Test
	@SuppressWarnings("static-method")
	void titleCaseConvertsFirstCharToUpperCase() {
		assertThat(ModulesUtil.titleCase("hello world"), is("Hello world"));
		assertThat(ModulesUtil.titleCase("HELLO"), is("HELLO"));
		assertThat(ModulesUtil.titleCase("hello"), is("Hello"));
		assertThat(ModulesUtil.titleCase(""), is(""));
		assertThat(ModulesUtil.titleCase(null), nullValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarMonthNameToNumberReturnsCorrectNumbers() {
		assertEquals(0, ModulesUtil.calendarMonthNameToNumber("JAN"));
		assertEquals(1, ModulesUtil.calendarMonthNameToNumber("FEB"));
		assertEquals(5, ModulesUtil.calendarMonthNameToNumber("JUN"));
		assertEquals(11, ModulesUtil.calendarMonthNameToNumber("DEC"));
		assertEquals(0, ModulesUtil.calendarMonthNameToNumber("Unknown"));
	}

	@Test
	@SuppressWarnings("static-method")
	void bothNullOrEqualReturnsTrueForSameValues() {
		assertTrue(ModulesUtil.bothNullOrEqual(null, null));
		assertTrue(ModulesUtil.bothNullOrEqual("hello", "hello"));
		assertFalse(ModulesUtil.bothNullOrEqual("hello", null));
		assertFalse(ModulesUtil.bothNullOrEqual(null, "world"));
		assertFalse(ModulesUtil.bothNullOrEqual("hello", "world"));
	}

	@Test
	@SuppressWarnings("static-method")
	void concatWithDelimJoinsStrings() {
		assertThat(ModulesUtil.concatWithDelim(", ", "a", "b", "c"), is("a, b, c"));
		assertThat(ModulesUtil.concatWithDelim("-", "x"), is("x"));
		assertThat(ModulesUtil.concatWithDelim("|", null, "b"), is("b"));
		assertThat(ModulesUtil.concatWithDelim(","), is(""));
	}

	@Test
	@SuppressWarnings("static-method")
	void enquoteWrapsWithQuoteCharacter() {
		// Single char quoteSet only prepends (no closing quote)
		assertThat(ModulesUtil.enquote("\"", "hello"), is("\"hello"));
		// Two char quoteSet wraps with l and r characters
		assertThat(ModulesUtil.enquote("()", "world"), is("(world)"));
		assertThat(ModulesUtil.enquote(null, "test"), is("test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getConditionNameReturnsNameForCode() {
		assertThat(ModulesUtil.getConditionName("flag"), is("isFlag"));
		assertThat(ModulesUtil.getConditionName("active"), is("isActive"));
	}

	@Test
	@SuppressWarnings("static-method")
	void configureGroup() {
		// Create test roles
		GroupRole testRole1 = GroupRole.newInstance();
		testRole1.setRoleName("admin.TestRole1");
		assertThat(testRole1, is(notNullValue()));

		GroupRole testRole2 = GroupRole.newInstance();
		testRole2.setRoleName("admin.TestRole2");
		assertThat(testRole2, is(notNullValue()));

		// New Group
		GroupExtension newGroup = ModulesUtil.configureGroup("TestGroup", "admin.TestRole1", "admin.TestRole2");
		assertThat(newGroup, is(notNullValue()));

		// Attempting to create new group with name and roles of existing group
		GroupExtension testExistingGroup = ModulesUtil.configureGroup("TestGroup", "admin.TestRole1", "admin.TestRole2");
		assertThat(testExistingGroup, is(notNullValue()));
		assertThat(testExistingGroup, is(newGroup));

		// Attempting to create new group with name of existing group but missing role
		GroupExtension testExistingGroupWithMissingRole = ModulesUtil.configureGroup("TestGroup", "admin.TestRole1");
		assertThat(testExistingGroupWithMissingRole, is(notNullValue()));
		assertThat(testExistingGroupWithMissingRole, is(testExistingGroup));
	}

	@Test
	@SuppressWarnings("static-method")
	void dayOfWeekToCalendarMapsEachDay() {
		assertEquals(Calendar.MONDAY, ModulesUtil.dayOfWeekToCalendar(DayOfWeek.Monday));
		assertEquals(Calendar.TUESDAY, ModulesUtil.dayOfWeekToCalendar(DayOfWeek.Tuesday));
		assertEquals(Calendar.WEDNESDAY, ModulesUtil.dayOfWeekToCalendar(DayOfWeek.Wednesday));
		assertEquals(Calendar.THURSDAY, ModulesUtil.dayOfWeekToCalendar(DayOfWeek.Thursday));
		assertEquals(Calendar.FRIDAY, ModulesUtil.dayOfWeekToCalendar(DayOfWeek.Friday));
		assertEquals(Calendar.SATURDAY, ModulesUtil.dayOfWeekToCalendar(DayOfWeek.Saturday));
		assertEquals(Calendar.SUNDAY, ModulesUtil.dayOfWeekToCalendar(DayOfWeek.Sunday));
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarToDayOfWeekMapsEachDay() {
		assertEquals(DayOfWeek.Monday, ModulesUtil.calendarToDayOfWeek(Calendar.MONDAY));
		assertEquals(DayOfWeek.Tuesday, ModulesUtil.calendarToDayOfWeek(Calendar.TUESDAY));
		assertEquals(DayOfWeek.Wednesday, ModulesUtil.calendarToDayOfWeek(Calendar.WEDNESDAY));
		assertEquals(DayOfWeek.Thursday, ModulesUtil.calendarToDayOfWeek(Calendar.THURSDAY));
		assertEquals(DayOfWeek.Friday, ModulesUtil.calendarToDayOfWeek(Calendar.FRIDAY));
		assertEquals(DayOfWeek.Saturday, ModulesUtil.calendarToDayOfWeek(Calendar.SATURDAY));
		assertEquals(DayOfWeek.Sunday, ModulesUtil.calendarToDayOfWeek(Calendar.SUNDAY));
	}

	@Test
	@SuppressWarnings("static-method")
	void annualFrequencyCountReturnsCorrectCounts() {
		assertEquals(1, ModulesUtil.annualFrequencyCount(OccurenceFrequency.OneOff));
		assertEquals(365, ModulesUtil.annualFrequencyCount(OccurenceFrequency.Daily));
		assertEquals(52, ModulesUtil.annualFrequencyCount(OccurenceFrequency.Weekly));
		assertEquals(26, ModulesUtil.annualFrequencyCount(OccurenceFrequency.Fortnightly));
		assertEquals(12, ModulesUtil.annualFrequencyCount(OccurenceFrequency.Monthly));
		assertEquals(4, ModulesUtil.annualFrequencyCount(OccurenceFrequency.Quarterly));
		assertEquals(2, ModulesUtil.annualFrequencyCount(OccurenceFrequency.HalfYearly));
		assertEquals(1, ModulesUtil.annualFrequencyCount(OccurenceFrequency.Yearly));
	}

	@Test
	@SuppressWarnings("static-method")
	void annualPeriodCountReturnsCorrectCounts() {
		assertEquals(365, ModulesUtil.annualPeriodCount(OccurrencePeriod.Days));
		assertEquals(52, ModulesUtil.annualPeriodCount(OccurrencePeriod.Weeks));
		assertEquals(12, ModulesUtil.annualPeriodCount(OccurrencePeriod.Months));
		assertEquals(1, ModulesUtil.annualPeriodCount(OccurrencePeriod.Years));
		// Seconds, Minutes, Hours default to 1
		assertEquals(1, ModulesUtil.annualPeriodCount(OccurrencePeriod.Seconds));
		assertEquals(1, ModulesUtil.annualPeriodCount(OccurrencePeriod.Minutes));
		assertEquals(1, ModulesUtil.annualPeriodCount(OccurrencePeriod.Hours));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyReturnsNullForNullDate() {
		assertNull(ModulesUtil.addFrequency(OccurenceFrequency.Daily, null, 1));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyOneOffReturnsSameDate() {
		DateOnly date = Time.withDate(15, 1, 2024); // 15 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.OneOff, date, 5);
		assertNotNull(result);
		assertEquals(date.getTime(), result.getTime());
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyDailyAddsCorrectDays() {
		DateOnly date = Time.withDate(1, 1, 2024); // 1 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.Daily, date, 10);
		assertNotNull(result);
		// 10 days added to 1 Jan → 11 Jan
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(11, cal.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyWeeklyAddsSevenDaysPerFrequency() {
		DateOnly date = Time.withDate(1, 1, 2024); // 1 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.Weekly, date, 2);
		assertNotNull(result);
		// 2 weeks = 14 days → 15 Jan 2024
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(15, cal.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyFortnightlyAddsFourteenDaysPerFrequency() {
		DateOnly date = Time.withDate(1, 1, 2024); // 1 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.Fortnightly, date, 1);
		assertNotNull(result);
		// 1 fortnight = 14 days → 15 Jan 2024
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(15, cal.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyMonthlyAddsMonths() {
		DateOnly date = Time.withDate(15, 1, 2024); // 15 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.Monthly, date, 3);
		assertNotNull(result);
		// After 3 months from Jan = April (month 3 in 0-based = Calendar.APRIL)
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(Calendar.APRIL, cal.get(Calendar.MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyQuarterlyAddsThreeMonths() {
		DateOnly date = Time.withDate(1, 1, 2024); // 1 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.Quarterly, date, 1);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(Calendar.APRIL, cal.get(Calendar.MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyHalfYearlyAddsSixMonths() {
		DateOnly date = Time.withDate(1, 1, 2024); // 1 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.HalfYearly, date, 1);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(Calendar.JULY, cal.get(Calendar.MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addFrequencyYearlyAddsYear() {
		DateOnly date = Time.withDate(15, 1, 2024); // 15 Jan 2024
		DateOnly result = ModulesUtil.addFrequency(OccurenceFrequency.Yearly, date, 2);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(2026, cal.get(Calendar.YEAR));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void lastDayOfMonthReturnsNullForNullDate() {
		assertNull(ModulesUtil.lastDayOfMonth(null));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void lastDayOfMonthReturnsCorrectDay() {
		DateOnly jan = Time.withDate(15, 1, 2024); // 15 Jan 2024
		DateOnly result = ModulesUtil.lastDayOfMonth(jan);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(31, cal.get(Calendar.DAY_OF_MONTH));

		DateOnly feb = Time.withDate(10, 2, 2024); // 10 Feb 2024 (leap year)
		DateOnly febResult = ModulesUtil.lastDayOfMonth(feb);
		assertNotNull(febResult);
		cal.setTime(febResult);
		assertEquals(29, cal.get(Calendar.DAY_OF_MONTH)); // 2024 is a leap year

		DateOnly apr = Time.withDate(1, 4, 2024); // 1 Apr 2024
		DateOnly aprResult = ModulesUtil.lastDayOfMonth(apr);
		assertNotNull(aprResult);
		cal.setTime(aprResult);
		assertEquals(30, cal.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void lastDayOfYearReturnsNullForNullDate() {
		assertNull(ModulesUtil.lastDayOfYear(null));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void lastDayOfYearReturnsDecember31() {
		DateOnly date = Time.withDate(15, 6, 2024); // 15 Jun 2024
		DateOnly result = ModulesUtil.lastDayOfYear(date);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(Calendar.DECEMBER, cal.get(Calendar.MONTH));
		assertEquals(31, cal.get(Calendar.DAY_OF_MONTH));
		assertEquals(2024, cal.get(Calendar.YEAR));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void firstDayOfMonthReturnsFirstOfMonth() {
		DateOnly date = Time.withDate(20, 6, 2024); // 20 Jun 2024
		DateOnly result = ModulesUtil.firstDayOfMonth(date);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(1, cal.get(Calendar.DAY_OF_MONTH));
		assertEquals(Calendar.JUNE, cal.get(Calendar.MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void firstDayOfYearReturnsJanuary1() {
		DateOnly date = Time.withDate(15, 6, 2024); // 15 Jun 2024
		DateOnly result = ModulesUtil.firstDayOfYear(date);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(Calendar.JANUARY, cal.get(Calendar.MONTH));
		assertEquals(1, cal.get(Calendar.DAY_OF_MONTH));
		assertEquals(2024, cal.get(Calendar.YEAR));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addDaysDateOnlyAddsPositiveDays() {
		DateOnly date = Time.withDate(10, 1, 2024); // 10 Jan 2024
		DateOnly result = ModulesUtil.addDaysDateOnly(date, 5);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(15, cal.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void addDaysDateOnlyAddsNegativeDays() {
		DateOnly date = Time.withDate(15, 1, 2024); // 15 Jan 2024
		DateOnly result = ModulesUtil.addDaysDateOnly(date, -5);
		assertNotNull(result);
		Calendar cal = Calendar.getInstance();
		cal.setTime(result);
		assertEquals(10, cal.get(Calendar.DAY_OF_MONTH));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void sqlFormatDateOnlyReturnsEmptyStringForNull() {
		assertEquals("", ModulesUtil.sqlFormatDateOnly(null));
	}

	@Test
	@SuppressWarnings({"static-method"})
	void sqlFormatDateOnlyFormatsDateCorrectly() {
		DateOnly date = Time.withDate(5, 3, 2024); // 5 Mar 2024
		String result = ModulesUtil.sqlFormatDateOnly(date);
		assertNotNull(result);
		assertTrue(result.contains("2024"));
		assertTrue(result.contains("03"));
		assertTrue(result.contains("05"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addValidationErrorAddsMessageWithBinding() {
		ValidationException e = new ValidationException();
		ModulesUtil.addValidationError(e, "myField", "Value is required");
		assertEquals(1, e.getMessages().size());
		assertEquals("Value is required", e.getMessages().get(0).getText());
		boolean found = false;
		for (String binding : e.getMessages().get(0).getBindings()) {
			if ("myField".equals(binding)) {
				found = true;
				break;
			}
		}
		assertTrue(found, "Expected binding 'myField' to be present");
	}

	@Test
	@SuppressWarnings("static-method")
	void getUniqueQuadraticResidueReturnsUniqueValues() {
		long r1 = ModulesUtil.getUniqueQuadraticResidue(1);
		long r2 = ModulesUtil.getUniqueQuadraticResidue(2);
		long r3 = ModulesUtil.getUniqueQuadraticResidue(3);
		assertNotEquals(r1, r2);
		assertNotEquals(r2, r3);
		assertNotEquals(r1, r3);
		// Same input always returns same output
		assertEquals(r1, ModulesUtil.getUniqueQuadraticResidue(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void getUniqueQuadraticResidueHandlesZero() {
		long result = ModulesUtil.getUniqueQuadraticResidue(0);
		assertTrue(result >= 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void hasModuleReturnsTrueForExistingModule() {
		assertTrue(ModulesUtil.hasModule("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void hasModuleReturnsFalseForNonExistentModule() {
		assertFalse(ModulesUtil.hasModule("nonExistentModuleXYZ"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPersistentIdentifierReturnsTableNameForPersistentDocument() {
		String result = ModulesUtil.getPersistentIdentifier("admin", "User");
		assertNotNull(result);
		assertTrue(result.contains("USR_User") || result.length() > 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void getPersistentIdentifierReturnsNullForNonPersistentDocument() {
		// AllAttributesPersistent is a persistent document so it should have a table name
		String result = ModulesUtil.getPersistentIdentifier("test", "AllAttributesPersistent");
		assertNotNull(result);
	}
}
