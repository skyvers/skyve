package modules.admin.JobSchedule;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.text.ParseException;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;

/**
 * Tests for JobCronExpression — pure Java, no H2 needed.
 */
@SuppressWarnings("static-method")
class JobCronExpressionTest {

	@Test
	void constructorWithValidExpressionDoesNotThrow() throws Exception {
		JobCronExpression expr = new JobCronExpression("0 0 12 * * ?");
		assertNotNull(expr);
	}

	@Test
	void constructorWithInvalidExpressionThrowsParseException() {
		assertThrows(ParseException.class, () -> new JobCronExpression("not a cron"));
	}

	@Test
	void getMinutesReturnsExpectedSet() throws Exception {
		// "0 15 10 * * ?" fires at 10:15am every day — minute field is 15
		JobCronExpression expr = new JobCronExpression("0 15 10 * * ?");
		Set<Integer> minutes = expr.getMinutes();
		assertNotNull(minutes);
		assertTrue(minutes.contains(15), "Expected minute 15");
	}

	@Test
	void getHoursReturnsExpectedSet() throws Exception {
		// "0 0 10 * * ?" fires at 10am — hour field is 10
		JobCronExpression expr = new JobCronExpression("0 0 10 * * ?");
		Set<Integer> hours = expr.getHours();
		assertNotNull(hours);
		assertTrue(hours.contains(10), "Expected hour 10");
	}

	@Test
	void getDaysOfMonthReturnsExpectedSet() throws Exception {
		// "0 0 12 1 * ?" fires at noon on the 1st of each month
		JobCronExpression expr = new JobCronExpression("0 0 12 1 * ?");
		Set<Integer> days = expr.getDaysOfMonth();
		assertNotNull(days);
		assertTrue(days.contains(1), "Expected day 1");
	}

	@Test
	void getMonthsReturnsExpectedSet() throws Exception {
		// "0 0 12 * 6 ?" fires at noon every day in June
		JobCronExpression expr = new JobCronExpression("0 0 12 * 6 ?");
		Set<Integer> months = expr.getMonths();
		assertNotNull(months);
		assertTrue(months.contains(6), "Expected month 6");
	}

	@Test
	void getDaysOfWeekReturnsExpectedSet() throws Exception {
		// "0 0 12 ? * MON" fires at noon every Monday
		JobCronExpression expr = new JobCronExpression("0 0 12 ? * MON");
		Set<Integer> daysOfWeek = expr.getDaysOfWeek();
		assertNotNull(daysOfWeek);
		assertFalse(daysOfWeek.isEmpty(), "Expected at least one day of week");
	}

	@Test
	void getLastDayOfWeekReturnsFalseForStandardExpression() throws Exception {
		JobCronExpression expr = new JobCronExpression("0 0 12 * * ?");
		assertFalse(expr.getLastDayOfWeek());
	}

	@Test
	void getLastDayOfMonthReturnsFalseForStandardExpression() throws Exception {
		JobCronExpression expr = new JobCronExpression("0 0 12 * * ?");
		assertFalse(expr.getLastDayOfMonth());
	}

	@Test
	void getNearestWeekdayReturnsFalseForStandardExpression() throws Exception {
		JobCronExpression expr = new JobCronExpression("0 0 12 * * ?");
		assertFalse(expr.getNearestWeekday());
	}

	@Test
	void getLastDayOfMonthReturnsTrueForLastDayExpression() throws Exception {
		// "0 0 12 L * ?" fires at noon on the last day of each month
		JobCronExpression expr = new JobCronExpression("0 0 12 L * ?");
		assertTrue(expr.getLastDayOfMonth());
	}

	@Test
	void getNearestWeekdayReturnsTrueForNearestWeekdayExpression() throws Exception {
		// "0 0 12 15W * ?" fires at noon on the nearest weekday to the 15th
		JobCronExpression expr = new JobCronExpression("0 0 12 15W * ?");
		assertTrue(expr.getNearestWeekday());
	}
}
