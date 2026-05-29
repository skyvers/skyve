package modules.admin.JobSchedule;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.domain.JobSchedule;
import modules.test.AbstractSkyveTest;

import org.junit.jupiter.api.Test;

/**
 * Tests for JobScheduleBizlet - covering getConstantDomainValues,
 * newInstance, preSave (cron expression building), and validate.
 */
@SuppressWarnings("static-method")
class JobScheduleBizletTest extends AbstractSkyveTest {

	private static JobScheduleBizlet bizlet = new JobScheduleBizlet();

	// ---- getConstantDomainValues ----

	@Test
	void getConstantDomainValuesForAllMinutesReturnsTwoEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(JobSchedule.allMinutesPropertyName);
		assertNotNull(result);
		assertEquals(2, result.size());
		assertEquals("*", result.get(0).getCode());
		assertEquals("X", result.get(1).getCode());
	}

	@Test
	void getConstantDomainValuesForAllHoursReturnsTwoEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(JobSchedule.allHoursPropertyName);
		assertNotNull(result);
		assertEquals(2, result.size());
	}

	@Test
	void getConstantDomainValuesForAllMonthsReturnsTwoEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(JobSchedule.allMonthsPropertyName);
		assertNotNull(result);
		assertEquals(2, result.size());
	}

	@Test
	void getConstantDomainValuesForAllWeekdaysReturnsTwoEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(JobSchedule.allWeekdaysPropertyName);
		assertNotNull(result);
		assertEquals(2, result.size());
	}

	@Test
	void getConstantDomainValuesForAllDaysReturnsFourEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(JobSchedule.allDaysPropertyName);
		assertNotNull(result);
		assertEquals(4, result.size());
		// Should include "L" and "LW" options
		boolean hasLastDay = result.stream().anyMatch(dv -> "L".equals(dv.getCode()));
		boolean hasLastWeekDay = result.stream().anyMatch(dv -> "LW".equals(dv.getCode()));
		assertTrue(hasLastDay);
		assertTrue(hasLastWeekDay);
	}

	@Test
	void getConstantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues("unknownAttribute");
		// should return null for unknown attribute names
		// (method returns null for non-matching attributes)
		assertTrue(result == null || result.isEmpty());
	}

	// ---- newInstance ----

	@Test
	void newInstanceSetsDefaultAllCodesOnBean() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		JobScheduleExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		assertEquals("*", result.getAllMinutes());
		assertEquals("*", result.getAllHours());
		assertEquals("*", result.getAllDays());
		assertEquals("*", result.getAllMonths());
		assertEquals("*", result.getAllWeekdays());
	}

	// ---- preSave (cron expression building) ----

	@Test
	void preSaveWithAllDefaultsBuildsAllStarCronExpression() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bizlet.preSave(bean);
		String cron = bean.getCronExpression();
		// Default: "0 * * * * ?"
		assertNotNull(cron);
		assertTrue(cron.startsWith("0 "), "Should start with '0 ' for seconds");
	}

	@Test
	void preSaveWithSelectedMinuteBuildsCronWithMinuteIndex() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllMinutes("X"); // Selected
		bean.setMinute5(Boolean.TRUE);
		bizlet.preSave(bean);
		String cron = bean.getCronExpression();
		assertNotNull(cron);
		assertTrue(cron.contains("5"), "Should contain minute 5");
	}

	@Test
	void preSaveWithSelectedHourBuildsCronWithHourIndex() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllHours("X");
		bean.setHour8(Boolean.TRUE);
		bizlet.preSave(bean);
		String cron = bean.getCronExpression();
		assertNotNull(cron);
		assertTrue(cron.contains("8"), "Should contain hour 8");
	}

	@Test
	void preSaveWithLastDayCodeSetsLastDay() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllDays("L");
		bizlet.preSave(bean);
		String cron = bean.getCronExpression();
		assertNotNull(cron);
		assertTrue(cron.contains("L"), "Should contain L for last day");
	}

	@Test
	void preSaveWithLastWeekDayCodeSetsLastWeekDay() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllDays("LW");
		bizlet.preSave(bean);
		String cron = bean.getCronExpression();
		assertNotNull(cron);
		assertTrue(cron.contains("LW"), "Should contain LW for last week day");
	}

	@Test
	void preSaveWithSelectedWeekdayAndAllDaysBuildsCronWithQuestion() throws Exception {
		// When allWeekdays is "X" (Selected), days of month becomes "?"
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllWeekdays("X");
		bean.setWeekday2(Boolean.TRUE);
		bizlet.preSave(bean);
		String cron = bean.getCronExpression();
		assertNotNull(cron);
		// When allDays=* and allWeekdays=X, days part should be "?"
		assertTrue(cron.contains("?"), "Should contain ? when weekday is selected");
	}

	// ---- validate ----

	@Test
	void validateWithAllDefaultsDoesNotAddMessages() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		assertTrue(ve.getMessages().isEmpty(), "No validation errors expected for default bean");
	}

	@Test
	void validateWithBothDaysAndWeekdaysSelectedAddsMessage() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllDays("X");
		bean.setAllWeekdays("X");
		bean.setDay1(Boolean.TRUE);
		bean.setWeekday1(Boolean.TRUE);
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		assertFalse(ve.getMessages().isEmpty(), "Should have validation error for days AND weekdays");
	}

	@Test
	void validateWithSelectedMinutesButNoneChosenAddsMessage() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllMinutes("X"); // Selected but no minutes set
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasMinsError = ve.getMessages().stream()
				.anyMatch(msg -> {
					for (String b : msg.getBindings()) {
						if (JobSchedule.allMinutesPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasMinsError, "Should have error for no minutes selected");
	}

	@Test
	void validateWithSelectedMinutesAndOneChosenHasNoMinuteError() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllMinutes("X");
		bean.setMinute0(Boolean.TRUE);
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasMinsError = ve.getMessages().stream()
				.anyMatch(msg -> {
					for (String b : msg.getBindings()) {
						if (JobSchedule.allMinutesPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertFalse(hasMinsError, "Should not have error when a minute is selected");
	}

	@Test
	void validateWithSelectedHoursButNoneChosenAddsMessage() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllHours("X");
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasHoursError = ve.getMessages().stream()
				.anyMatch(msg -> {
					for (String b : msg.getBindings()) {
						if (JobSchedule.allHoursPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasHoursError, "Should have error for no hours selected");
	}

	@Test
	void validateWithSelectedDaysButNoneChosenAddsMessage() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllDays("X");
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasDaysError = ve.getMessages().stream()
				.anyMatch(msg -> {
					for (String b : msg.getBindings()) {
						if (JobSchedule.allDaysPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasDaysError, "Should have error for no days selected");
	}

	@Test
	void validateWithSelectedMonthsButNoneChosenAddsMessage() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllMonths("X");
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasMonthsError = ve.getMessages().stream()
				.anyMatch(msg -> {
					for (String b : msg.getBindings()) {
						if (JobSchedule.allMonthsPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasMonthsError, "Should have error for no months selected");
	}

	@Test
	void validateWithSelectedWeekdaysButNoneChosenAddsMessage() throws Exception {
		JobScheduleExtension bean = JobSchedule.newInstance();
		bizlet.newInstance(bean);
		bean.setAllWeekdays("X");
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasWeekdaysError = ve.getMessages().stream()
				.anyMatch(msg -> {
					for (String b : msg.getBindings()) {
						if (JobSchedule.allWeekdaysPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasWeekdaysError, "Should have error for no weekdays selected");
	}
}
