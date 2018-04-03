package modules.admin.DataMaintenance;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.DataMaintenance;
import modules.admin.util.DataMaintenanceFactory;

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
