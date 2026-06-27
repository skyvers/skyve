package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import static org.junit.jupiter.api.Assertions.assertNull;

import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.EvictOption;
import modules.admin.domain.DataMaintenance.RefreshOption;
import modules.admin.domain.DataMaintenance.RestoreIndexingOption;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import util.AbstractH2Test;

/**
 * Tests for the {@link DataMaintenance} admin domain bean (persistent).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class DataMaintenanceDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesDataMaintenanceBean() {
		DataMaintenance bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(DataMaintenance.MODULE_NAME, DataMaintenance.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() {
		DataMaintenance bean = DataMaintenance.newInstance();
		assertEquals(DataMaintenance.MODULE_NAME, bean.getBizModule());
		assertEquals(DataMaintenance.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void backupRetentionSetAndGet() {
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
	void restorePreProcessSetAndGet() {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setRestorePreProcess(RestorePreProcess.noProcessing);
		assertEquals(RestorePreProcess.noProcessing, bean.getRestorePreProcess());
	}

	@Test
	void contentRestoreOptionSetAndGet() {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setContentRestoreOption(ContentRestoreOption.error);
		assertEquals(ContentRestoreOption.error, bean.getContentRestoreOption());
	}

	@Test
	void evictOptionSetAndGet() {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setEvictOption(EvictOption.all);
		assertEquals(EvictOption.all, bean.getEvictOption());
		bean.setEvictOption(EvictOption.none);
		assertEquals(EvictOption.none, bean.getEvictOption());
		bean.setEvictOption(EvictOption.bean);
		assertEquals(EvictOption.bean, bean.getEvictOption());
	}

	@Test
	void selectedBackupNameSetAndGet() {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSelectedBackupName("backup-20250101.zip");
		assertEquals("backup-20250101.zip", bean.getSelectedBackupName());
	}

	@Test
	void notificationSetAndGet() {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setNotification(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getNotification());
	}

	@Test
	void refreshFlagsSetAndGet() {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setRefreshBackups(Boolean.FALSE);
		bean.setRefreshContent(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getRefreshBackups());
		assertEquals(Boolean.FALSE, bean.getRefreshContent());
	}

	@Test
	void auditFieldsSetAndGet() {
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
	void schemaNameAndDdlScriptSetAndGet() {
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSchemaName("skyve");
		bean.setDdlScript("CREATE TABLE test (id bigint)");
		assertEquals("skyve", bean.getSchemaName());
		assertEquals("CREATE TABLE test (id bigint)", bean.getDdlScript());
	}

        @Test
        void restorePreProcessFromCodeAndFromLocalisedDescription() {
                assertEquals(RestorePreProcess.noProcessing, RestorePreProcess.fromCode("noProcessing"));
                assertNull(RestorePreProcess.fromCode("notexist"));
                assertNull(RestorePreProcess.fromLocalisedDescription("notexist"));
                assertNotNull(RestorePreProcess.fromLocalisedDescription(RestorePreProcess.noProcessing.toLocalisedDescription()));
        }

        @Test
        void restorePreProcessToDomainValues() {
                assertNotNull(RestorePreProcess.toDomainValues());
                assertEquals(8, RestorePreProcess.toDomainValues().size());
        }

        @Test
        void restoreIndexingOptionFromCodeAndFromLocalisedDescription() {
                assertEquals(RestoreIndexingOption.data, RestoreIndexingOption.fromCode("data"));
                assertNull(RestoreIndexingOption.fromCode("notexist"));
                assertNull(RestoreIndexingOption.fromLocalisedDescription("notexist"));
                assertNotNull(RestoreIndexingOption.fromLocalisedDescription(RestoreIndexingOption.data.toLocalisedDescription()));
        }

        @Test
        void restoreIndexingOptionToDomainValues() {
                assertNotNull(RestoreIndexingOption.toDomainValues());
                assertEquals(4, RestoreIndexingOption.toDomainValues().size());
        }

        @Test
        void contentRestoreOptionFromCodeAndFromLocalisedDescription() {
                assertEquals(ContentRestoreOption.error, ContentRestoreOption.fromCode("error"));
                assertNull(ContentRestoreOption.fromCode("notexist"));
                assertNull(ContentRestoreOption.fromLocalisedDescription("notexist"));
                assertNotNull(ContentRestoreOption.fromLocalisedDescription(ContentRestoreOption.error.toLocalisedDescription()));
        }

        @Test
        void contentRestoreOptionToDomainValues() {
                assertNotNull(ContentRestoreOption.toDomainValues());
                assertEquals(3, ContentRestoreOption.toDomainValues().size());
        }

        @Test
        void restorePreProcessToCodeAndToDomainValue() {
                assertNotNull(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromBackupCreatesql.toCode());
                assertNotNull(RestorePreProcess.dropTablesUsingMetadataRecreateTablesFromBackupCreatesql.toDomainValue());
        }

        @Test
        void restoreIndexingOptionToCodeAndToDomainValue() {
                assertNotNull(RestoreIndexingOption.data.toCode());
                assertNotNull(RestoreIndexingOption.data.toDomainValue());
        }

        @Test
        void contentRestoreOptionToCodeAndToDomainValue() {
                assertNotNull(ContentRestoreOption.error.toCode());
                assertNotNull(ContentRestoreOption.error.toDomainValue());
        }

	@Test
	void evictOptionFromCodeAndFromLocalisedDescription() {
		assertEquals(EvictOption.bean, EvictOption.fromCode("Bean"));
		assertNull(EvictOption.fromCode("nonexistent"));
		assertNotNull(EvictOption.fromLocalisedDescription(EvictOption.bean.toLocalisedDescription()));
		assertNull(EvictOption.fromLocalisedDescription("nonexistent"));
		assertNotNull(EvictOption.toDomainValues());
	}

	@Test
	void refreshOptionFromCodeAndFromLocalisedDescription() {
		assertEquals(RefreshOption.upsert, RefreshOption.fromCode("Upsert"));
		assertNull(RefreshOption.fromCode("nonexistent"));
		assertNotNull(RefreshOption.fromLocalisedDescription(RefreshOption.upsert.toLocalisedDescription()));
		assertNull(RefreshOption.fromLocalisedDescription("nonexistent"));
		assertNotNull(RefreshOption.toDomainValues());
	}
}
