package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.EvictOption;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import util.AbstractH2Test;

/**
 * Tests for the {@link DataMaintenance} admin domain bean (persistent).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class DataMaintenanceDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesDataMaintenanceBean() throws Exception {
		DataMaintenance bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		assertEquals(DataMaintenance.MODULE_NAME, bean.getBizModule());
		assertEquals(DataMaintenance.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void backupRetentionSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setDailyBackupRetention(Integer.valueOf(7));
		bean.setWeeklyBackupRetention(Integer.valueOf(4));
		bean.setMonthlyBackupRetention(Integer.valueOf(12));
		bean.setYearlyBackupRetention(Integer.valueOf(3));
		assertEquals(Integer.valueOf(7), bean.getDailyBackupRetention());
		assertEquals(Integer.valueOf(4), bean.getWeeklyBackupRetention());
		assertEquals(Integer.valueOf(12), bean.getMonthlyBackupRetention());
		assertEquals(Integer.valueOf(3), bean.getYearlyBackupRetention());
	}

	@Test
	void restorePreProcessSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setRestorePreProcess(RestorePreProcess.noProcessing);
		assertEquals(RestorePreProcess.noProcessing, bean.getRestorePreProcess());
	}

	@Test
	void contentRestoreOptionSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setContentRestoreOption(ContentRestoreOption.error);
		assertEquals(ContentRestoreOption.error, bean.getContentRestoreOption());
	}

	@Test
	void evictOptionSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setEvictOption(EvictOption.all);
		assertEquals(EvictOption.all, bean.getEvictOption());
		bean.setEvictOption(EvictOption.none);
		assertEquals(EvictOption.none, bean.getEvictOption());
		bean.setEvictOption(EvictOption.bean);
		assertEquals(EvictOption.bean, bean.getEvictOption());
	}

	@Test
	void selectedBackupNameSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSelectedBackupName("backup-20250101.zip");
		assertEquals("backup-20250101.zip", bean.getSelectedBackupName());
	}

	@Test
	void notificationSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setNotification(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getNotification());
	}

	@Test
	void refreshFlagsSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setRefreshBackups(Boolean.FALSE);
		bean.setRefreshContent(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getRefreshBackups());
		assertEquals(Boolean.FALSE, bean.getRefreshContent());
	}

	@Test
	void auditFieldsSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setAuditModuleName("admin");
		bean.setAuditDocumentName("User");
		Timestamp ts = new Timestamp();
		bean.setAuditTimestampStart(ts);
		assertEquals("admin", bean.getAuditModuleName());
		assertEquals("User", bean.getAuditDocumentName());
		assertNotNull(bean.getAuditTimestampStart());
	}

	@Test
	void schemaNameAndDdlScriptSetAndGet() throws Exception {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSchemaName("skyve");
		bean.setDdlScript("CREATE TABLE test (id bigint)");
		assertEquals("skyve", bean.getSchemaName());
		assertEquals("CREATE TABLE test (id bigint)", bean.getDdlScript());
	}
}
