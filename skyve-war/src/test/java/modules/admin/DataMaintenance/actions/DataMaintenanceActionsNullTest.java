package modules.admin.DataMaintenance.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.DataMaintenance.DataMaintenanceExtension;

@SuppressWarnings("static-method")
class DataMaintenanceActionsNullTest {

	@Test
	void truncateWithNullConfirmPasswordThrowsValidationException() {
		Truncate action = new Truncate();
		DataMaintenanceExtension bean = new DataMaintenanceExtension();
		// confirmPassword is null → throws before calling CDI userService
		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}

	@Test
	void refreshDocumentTuplesWithNullRefreshOptionThrowsValidationException() {
		RefreshDocumentTuples action = new RefreshDocumentTuples();
		DataMaintenanceExtension bean = new DataMaintenanceExtension();
		// refreshOption is null → throws before calling CORE
		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}
}
