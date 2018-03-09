package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.DataMaintenance;

@SkyveFactory(testAction = false)
public class DataMaintenanceFactoryExtension extends DataMaintenanceFactory {
	@Override
	public DataMaintenance getInstance() throws Exception {
		DataMaintenance result = super.getInstance();
		result.setModDocName(null);
		result.setRestorePreProcess(null);
		result.setAuditModuleName(null);
		result.setAuditDocumentName(null);
		return result;
	}
}
