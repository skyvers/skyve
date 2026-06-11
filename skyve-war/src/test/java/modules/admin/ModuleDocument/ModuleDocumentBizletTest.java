package modules.admin.ModuleDocument;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.admin.DataMaintenance.DataMaintenanceExtension;

@SuppressWarnings("static-method")
class ModuleDocumentBizletTest {

	private static final ModuleDocumentBizlet bizlet = new ModuleDocumentBizlet();

	@Test
	void resolveWithNullConversationBeanReturnsNull() throws Exception {
		assertNull(bizlet.resolve("some-biz-id", null, null));
	}

	@Test
	void resolveWithNonControlPanelConversationBeanReturnsNull() throws Exception {
		// DataMaintenanceExtension is not a ControlPanelExtension, so resolve should fall to super
		DataMaintenanceExtension nonCp = new DataMaintenanceExtension();
		assertNull(bizlet.resolve("some-biz-id", nonCp, null));
	}
}
