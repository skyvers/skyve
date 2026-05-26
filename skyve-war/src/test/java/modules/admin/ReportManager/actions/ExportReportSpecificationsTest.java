package modules.admin.ReportManager.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.ReportManager.ReportManagerExtension;

/**
 * Tests for ExportReportSpecifications action.
 */
@SuppressWarnings("static-method")
class ExportReportSpecificationsTest {

	@Test
	void prepareWithEmptyCurrentReportsThrowsValidationException() throws Exception {
		ExportReportSpecifications action = new ExportReportSpecifications();
		ReportManagerExtension bean = new ReportManagerExtension();
		// currentReports is initialized to an empty ChangeTrackingArrayList
		// so size() == 0 → throws ValidationException before any CDI call
		assertThrows(ValidationException.class, () -> action.prepare(bean, null));
	}
}
