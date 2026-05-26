package modules.admin.ReportTemplate;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportTemplate;
import modules.test.AbstractSkyveTest;

/**
 * Tests for ReportTemplateBizlet covering getConstantDomainValues and validate.
 */
public class ReportTemplateBizletTest extends AbstractSkyveTest {

	private static ReportTemplateBizlet bizlet = new ReportTemplateBizlet();

	// ---- getConstantDomainValues for scheduling attributes ----

	@Test
	void getConstantDomainValuesForAllHoursReturnsTwoEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(ReportTemplate.allHoursPropertyName);
		assertNotNull(result);
		assertFalse(result.isEmpty());
		assertTrue(result.stream().anyMatch(dv -> "*".equals(dv.getCode())));
		assertTrue(result.stream().anyMatch(dv -> "X".equals(dv.getCode())));
	}

	@Test
	void getConstantDomainValuesForAllMonthsReturnsTwoEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(ReportTemplate.allMonthsPropertyName);
		assertNotNull(result);
		assertFalse(result.isEmpty());
	}

	@Test
	void getConstantDomainValuesForAllWeekdaysReturnsTwoEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(ReportTemplate.allWeekdaysPropertyName);
		assertNotNull(result);
		assertFalse(result.isEmpty());
	}

	@Test
	void getConstantDomainValuesForAllDaysReturnsFourEntries() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(ReportTemplate.allDaysPropertyName);
		assertNotNull(result);
		// Should return 4 values: All, Last Day, Last Week Day, Selected
		assertTrue(result.size() >= 4, "Should have at least 4 entries");
		assertTrue(result.stream().anyMatch(dv -> "L".equals(dv.getCode())));
		assertTrue(result.stream().anyMatch(dv -> "LW".equals(dv.getCode())));
	}

	// ---- validate ----

	@Test
	void validateWhenNotScheduledAddsNoMessages() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.FALSE);
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		// When not scheduled, no scheduling errors should be added
		boolean hasScheduleError = ve.getMessages().stream()
				.anyMatch(m -> m.getText() != null && m.getText().contains("scheduled"));
		assertFalse(hasScheduleError, "No schedule-related errors expected when not scheduled");
	}

	@Test
	void validateWhenScheduledWithRequiredParametersAddsMessage() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.TRUE);
		// Add a required parameter
		ReportParameterExtension param = new ReportParameterExtension();
		param.setRequired(Boolean.TRUE);
		param.setName("testParam");
		bean.getParameters().add(param);
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasRequiredError = ve.getMessages().stream()
				.anyMatch(m -> m.getText() != null && m.getText().contains("required parameters"));
		assertTrue(hasRequiredError, "Should have error about required parameters");
	}

	@Test
	void validateWhenScheduledWithNoUsersToEmailAddsMessage() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.TRUE);
		// No users to email - users list should be empty by default
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasUsersError = ve.getMessages().stream()
				.anyMatch(m -> {
					for (String b : m.getBindings()) {
						if (ReportTemplate.usersToEmailPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasUsersError, "Should have error about no users to email");
	}

	@Test
	void validateWhenScheduledWithSelectedHoursButNoneChosenAddsMessage() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.TRUE);
		bean.setAllHours("X"); // Selected but no hours chosen
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasHoursError = ve.getMessages().stream()
				.anyMatch(m -> {
					for (String b : m.getBindings()) {
						if (ReportTemplate.allHoursPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasHoursError, "Should have error for no hours selected");
	}

	@Test
	void validateWhenScheduledWithSelectedDaysButNoneChosenAddsMessage() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.TRUE);
		bean.setAllDays("X"); // Selected but no days chosen
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasDaysError = ve.getMessages().stream()
				.anyMatch(m -> {
					for (String b : m.getBindings()) {
						if (ReportTemplate.allDaysPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasDaysError, "Should have error for no days selected");
	}

	@Test
	void validateWhenScheduledWithSelectedMonthsButNoneChosenAddsMessage() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.TRUE);
		bean.setAllMonths("X"); // Selected but no months chosen
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasMonthsError = ve.getMessages().stream()
				.anyMatch(m -> {
					for (String b : m.getBindings()) {
						if (ReportTemplate.allMonthsPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasMonthsError, "Should have error for no months selected");
	}

	@Test
	void validateWhenScheduledWithSelectedWeekdaysButNoneChosenAddsMessage() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.TRUE);
		bean.setAllWeekdays("X"); // Selected but no weekdays chosen
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasWeekdaysError = ve.getMessages().stream()
				.anyMatch(m -> {
					for (String b : m.getBindings()) {
						if (ReportTemplate.allWeekdaysPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasWeekdaysError, "Should have error for no weekdays selected");
	}

	@Test
	void validateWhenScheduledWithBothDaysAndWeekdaysSelectedAddsMessage() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.TRUE);
		bean.setAllDays("X");
		bean.setAllWeekdays("X");
		bean.setDay1(Boolean.TRUE);
		bean.setWeekday1(Boolean.TRUE);
		ValidationException ve = new ValidationException();
		bizlet.validate(bean, ve);
		boolean hasBothError = ve.getMessages().stream()
				.anyMatch(m -> {
					for (String b : m.getBindings()) {
						if (ReportTemplate.allDaysPropertyName.equals(b)) return true;
						if (ReportTemplate.allWeekdaysPropertyName.equals(b)) return true;
					}
					return false;
				});
		assertTrue(hasBothError, "Should have error when both days and weekdays are selected");
	}

	// ---- getDynamicDomainValues ----

	@Test
	void getDynamicDomainValuesForDocumentNameWithNullModuleReturnsEmptyList() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setModuleName(null);
		List<DomainValue> result = bizlet.getDynamicDomainValues(ReportTemplate.documentNamePropertyName, bean);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void getDynamicDomainValuesForGenerateDocumentNameWithNullModuleReturnsEmptyList() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setGenerateModuleName(null);
		List<DomainValue> result = bizlet.getDynamicDomainValues(ReportTemplate.generateDocumentNamePropertyName, bean);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	// ---- preRerender ----

	@Test
	void preRerenderWithReportTypeSourceClearsGenerateFields() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setGenerateExisting(ReportTemplate.GenerateExisting.generate);
		bean.setGenerateModuleName("admin");
		bean.setGenerateDocumentName("User");
		bizlet.preRerender(ReportTemplate.reportTypePropertyName, bean, null);
		// Should clear the generate fields
		assertNull(bean.getGenerateExisting());
		assertNull(bean.getGenerateModuleName());
		assertNull(bean.getGenerateDocumentName());
	}

	@Test
	void preRerenderWithScheduledFalseDoesNotThrow() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bean.setScheduled(Boolean.FALSE);
		// Should not throw any exception
		bizlet.preRerender(ReportTemplate.scheduledPropertyName, bean, null);
	}

	@Test
	void preRerenderWithUnknownSourceDoesNotThrow() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		bizlet.preRerender("unknownSource", bean, null);
	}

	// ---- newInstance ----

	@Test
	void newInstanceSetsDefaultSchedulingCodes() throws Exception {
		ReportTemplateExtension bean = (ReportTemplateExtension) ReportTemplate.newInstance();
		ReportTemplateExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		// All scheduling fields should be set to "*" (All)
		assertTrue("*".equals(result.getAllHours()), "allHours should be '*'");
		assertTrue("*".equals(result.getAllDays()), "allDays should be '*'");
		assertTrue("*".equals(result.getAllMonths()), "allMonths should be '*'");
		assertTrue("*".equals(result.getAllWeekdays()), "allWeekdays should be '*'");
	}
}
