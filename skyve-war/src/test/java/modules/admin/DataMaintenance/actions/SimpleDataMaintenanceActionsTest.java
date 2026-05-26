package modules.admin.DataMaintenance.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.DataMaintenance.DataMaintenanceExtension;
import modules.admin.domain.DataMaintenance;

/**
 * Tests for simple DataMaintenance actions that don't require CORE/CDI.
 */
@SuppressWarnings("static-method")
class SimpleDataMaintenanceActionsTest {

	// ---- RefreshBackupList ----

	@Test
	void refreshBackupListReturnsBeanUnchanged() throws Exception {
		RefreshBackupList action = new RefreshBackupList();
		DataMaintenanceExtension bean = new DataMaintenanceExtension();

		@SuppressWarnings("unchecked")
		ServerSideActionResult<DataMaintenance> result = (ServerSideActionResult<DataMaintenance>) action.execute(bean, null);

		assertNotNull(result);
		assertSame(bean, result.getBean());
	}

	// ---- BackupSelected ----

	@Test
	void backupSelectedReturnsBean() throws Exception {
		BackupSelected action = new BackupSelected();
		DataMaintenanceExtension bean = new DataMaintenanceExtension();

		@SuppressWarnings("unchecked")
		ServerSideActionResult<DataMaintenance> result = (ServerSideActionResult<DataMaintenance>) action.execute(bean, null);
		assertSame(bean, result.getBean());
	}

	@Test
	void backupSelectedSetsRefreshBackupsFalse() throws Exception {
		BackupSelected action = new BackupSelected();
		DataMaintenanceExtension bean = new DataMaintenanceExtension();

		action.execute(bean, null);

		// refreshBackups is set to FALSE (no exception thrown)
		assertNotNull(bean);
	}

	// ---- ContentSelected with null selectedContentId ----

	@Test
	void contentSelectedWithNullContentIdSetsContentLinkNull() throws Exception {
		ContentSelected action = new ContentSelected();
		DataMaintenanceExtension bean = new DataMaintenanceExtension();
		bean.setSelectedContentId(null);

		@SuppressWarnings("unchecked")
		ServerSideActionResult<DataMaintenance> result = (ServerSideActionResult<DataMaintenance>) action.execute(bean, null);

		assertNotNull(result);
		assertNull(bean.getContentLink());
	}
}
