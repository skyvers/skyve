package modules.admin.DataMaintenance;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.AdminDocument;

/**
 * Creates Data Maintenance documents with default maintenance state.
 */
@SkyveFactory(testAction = false)
public class DataMaintenanceFactory {
	/**
	 * Performs the crudInstance operation.
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static DataMaintenanceExtension crudInstance() {
		DataMaintenanceExtension bean = new DataBuilder()
				.optional(true, false)
				.factoryBuild(AdminDocument.DATA_MAINTENANCE);

		bean.setModDocName(null);
		bean.setRestorePreProcess(null);
		bean.setAuditModuleName(null);
		bean.setAuditDocumentName(null);

		return bean;
	}
}
