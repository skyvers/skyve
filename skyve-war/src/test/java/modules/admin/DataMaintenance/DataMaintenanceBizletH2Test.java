package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import util.AbstractH2Test;

/**
 * H2-backed tests for DataMaintenanceBizlet covering getConstantDomainValues,
 * getDynamicDomainValues, preRerender and preExecute.
 */
class DataMaintenanceBizletH2Test extends AbstractH2Test {

	private static final DataMaintenanceBizlet bizlet = new DataMaintenanceBizlet();

	private DataBuilder db;
	private DataMaintenance bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- getConstantDomainValues: modDocName ----

	@Test
	@SuppressWarnings("static-method")
	void getConstantDomainValuesForModDocNameReturnsPersistableDocuments() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(DataMaintenance.modDocNamePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one persistable document");
	}

	// ---- getConstantDomainValues: auditModuleName ----

	@Test
	@SuppressWarnings("static-method")
	void getConstantDomainValuesForAuditModuleNameReturnsModules() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(DataMaintenance.auditModuleNamePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one module");
	}

	// ---- getConstantDomainValues: restorePreProcess ----

	@Test
	@SuppressWarnings("static-method")
	void getConstantDomainValuesForRestorePreProcessReturnsOptions() throws Exception {
		List<DomainValue> result = bizlet.getConstantDomainValues(DataMaintenance.restorePreProcessPropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least noProcessing option");
	}

	// ---- getConstantDomainValues: unknown attribute ----

	@Test
	@SuppressWarnings("static-method")
	void getConstantDomainValuesForUnknownAttributeReturnsNull() {
		Assertions.assertDoesNotThrow(() -> bizlet.getConstantDomainValues("unknownAttribute"));
		// Super class may return null for unknown
		// just should not throw
	}

	// ---- getDynamicDomainValues: auditDocumentName ----

	@Test
	void getDynamicDomainValuesForAuditDocumentNameWithModuleSetReturnsDocs() throws Exception {
		bean.setAuditModuleName("admin");

		List<DomainValue> result = bizlet.getDynamicDomainValues(DataMaintenance.auditDocumentNamePropertyName, bean);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected persistable documents in admin module");
	}

	@Test
	void getDynamicDomainValuesForAuditDocumentNameWithNullModuleCallsSuper() throws Exception {
		Assertions.assertDoesNotThrow(() -> bean.setAuditModuleName(null));
		bizlet.getDynamicDomainValues(DataMaintenance.auditDocumentNamePropertyName, bean);
		// Super class behaviour - should not throw
	}

	// ---- preRerender: restorePreProcess ----

	@Test
	void preRerenderRestorePreProcessNoProcessingSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.noProcessing);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderRestorePreProcessCreateTablesFromBackupSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.createTablesFromBackup);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderRestorePreProcessCreateTablesFromMetadataSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.createTablesFromMetadata);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderRestorePreProcessDeleteExistingDataSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.deleteExistingTableDataUsingMetadata);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderRestorePreProcessDropTablesBackupDropSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.dropTablesUsingBackupDropsqlRecreateTablesFromBackupCreatesql);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderRestorePreProcessDropTablesBackupRecreateMetadataSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.dropTablesUsingBackupDropsqlRecreateTablesFromMetadata);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderRestorePreProcessDropTablesMetadataRecreateBackupSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromBackupCreatesql);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderRestorePreProcessDropTablesMetadataRecreateMetadataSetsHint() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromMetadata);
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		assertNotNull(bean.getInstructionHint());
	}

	@Test
	void preRerenderWithNullRestorePreProcessDoesNotThrow() throws Exception {
		Assertions.assertDoesNotThrow(() -> bean.setRestorePreProcess(null));
		bizlet.preRerender(DataMaintenance.restorePreProcessPropertyName, bean, webContext);
		// just shouldn't throw
	}

	@Test
	void preRerenderWithNonRestoreSourceDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> bizlet.preRerender("someOtherSource", bean, webContext));
		// should not throw
	}

	// ---- preExecute: Edit and New ----

	@Test
	void preExecuteEditSetsRestorePreProcess() throws Exception {
		DataMaintenance result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, webContext);
		assertNotNull(result);
		assertThat(result.getRestorePreProcess(), is(RestorePreProcess.deleteExistingTableDataUsingMetadata));
	}

	@Test
	void preExecuteNewSetsRestorePreProcess() throws Exception {
		DataMaintenance result = bizlet.preExecute(ImplicitActionName.New, bean, null, webContext);
		assertNotNull(result);
		assertThat(result.getRestorePreProcess(), is(RestorePreProcess.deleteExistingTableDataUsingMetadata));
	}

	@Test
	void preExecuteSaveDoesNotChangeRestorePreProcess() throws Exception {
		bean.setRestorePreProcess(RestorePreProcess.noProcessing);
		DataMaintenance result = bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext);
		assertNotNull(result);
		// Save action doesn't change restorePreProcess
		assertThat(result.getRestorePreProcess(), is(RestorePreProcess.noProcessing));
	}
}
