package modules.admin.DataMaintenance;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.backup.RestoreOptions;

import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.RestoreIndexingOption;
import modules.admin.domain.DataMaintenance.RestorePreProcess;

@SuppressWarnings("static-method")
class DataMaintenanceExtensionTest {

	@Test
	void getPreProcessMapsNoProcessing() {
		DataMaintenanceExtension ext = new DataMaintenanceExtension();
		ext.setRestorePreProcess(RestorePreProcess.noProcessing);
		assertEquals(RestoreOptions.PreProcess.noProcessing, ext.getPreProcess());
	}

	@Test
	void getContentOptionMapsClearOrphanedContentIDs() {
		DataMaintenanceExtension ext = new DataMaintenanceExtension();
		ext.setContentRestoreOption(ContentRestoreOption.clearOrphanedContentIDs);
		assertEquals(RestoreOptions.ContentOption.clearOrphanedContentIds, ext.getContentOption());
	}

	@Test
	void getIndexingOptionMapsData() {
		DataMaintenanceExtension ext = new DataMaintenanceExtension();
		ext.setRestoreIndexingOption(RestoreIndexingOption.data);
		assertEquals(RestoreOptions.IndexingOption.data, ext.getIndexingOption());
	}
}
