package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ImportExport.Mode;
import modules.admin.domain.ImportExport.RollbackErrors;
import util.AbstractH2Test;

/**
 * Tests for the {@link ImportExport} admin domain bean (persistent).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class ImportExportDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesImportExportBean() {
		ImportExport bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ImportExport.MODULE_NAME, ImportExport.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() {
		ImportExport bean = ImportExport.newInstance();
		assertEquals(ImportExport.MODULE_NAME, bean.getBizModule());
		assertEquals(ImportExport.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void modeSetAndGet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.importData);
		assertEquals(Mode.importData, bean.getMode());
	}

	@Test
	void modeExportSetAndGet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.exportData);
		assertEquals(Mode.exportData, bean.getMode());
	}

	@Test
	void rollbackErrorsSetAndGet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setRollbackErrors(RollbackErrors.rollbackErrors);
		assertEquals(RollbackErrors.rollbackErrors, bean.getRollbackErrors());
	}

	@Test
	void moduleAndDocumentNameSetAndGet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setModuleName("admin");
		bean.setDocumentName("User");
		assertEquals("admin", bean.getModuleName());
		assertEquals("User", bean.getDocumentName());
	}

	@Test
	void filePathsSetAndGet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setImportFileName("data.csv");
		bean.setImportFileAbsolutePath("/tmp/data.csv");
		bean.setExportFileAbsolutePath("/tmp/export.xlsx");
		assertEquals("data.csv", bean.getImportFileName());
		assertEquals("/tmp/data.csv", bean.getImportFileAbsolutePath());
		assertEquals("/tmp/export.xlsx", bean.getExportFileAbsolutePath());
	}

	@Test
	@SuppressWarnings("deprecation")
	void booleanFlagsSetAndGet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setColumnTitlesOnly(Boolean.TRUE);
		bean.setAdvancedMode(Boolean.FALSE);
		assertEquals(Boolean.TRUE, bean.getColumnTitlesOnly());
		assertEquals(Boolean.FALSE, bean.getAdvancedMode());
	}

	@Test
	void resultsSetAndGet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setResults("Imported: 10 records");
		assertEquals("Imported: 10 records", bean.getResults());
	}

	@Test
	void getBizKeyNotNull() {
		ImportExport bean = ImportExport.newInstance();
		assertNotNull(bean.getBizKey());
	}

	@Test
	void modeFromCode() {
		assertEquals(Mode.importData, Mode.fromCode("importData"));
		assertEquals(Mode.exportData, Mode.fromCode("exportData"));
	}

	@Test
	void modeFromCodeUnknownReturnsNull() {
		assertNull(Mode.fromCode("notexist"));
	}

	@Test
	void modeToLocalisedDescription() {
		assertNotNull(Mode.importData.toLocalisedDescription());
	}

	@Test
	void modeToDomainValues() {
		assertEquals(2, Mode.toDomainValues().size());
	}

	@Test
	void modeFromLocalisedDescription() {
		assertNotNull(Mode.fromLocalisedDescription(Mode.importData.toLocalisedDescription()));
	}

	@Test
	void modeFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Mode.fromLocalisedDescription("notexist"));
	}

	@Test
	void rollbackErrorsFromCode() {
		assertEquals(RollbackErrors.rollbackErrors, RollbackErrors.fromCode("rollbackErrors"));
	}

	@Test
	void rollbackErrorsFromCodeUnknownReturnsNull() {
		assertNull(RollbackErrors.fromCode("notexist"));
	}

	@Test
	void rollbackErrorsToDomainValues() {
		assertEquals(2, RollbackErrors.toDomainValues().size());
	}

	@Test
	void rollbackErrorsFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(RollbackErrors.fromLocalisedDescription("notexist"));
	}

	@Test
	void isContextSetFalseWhenModuleAndDocumentAreNull() {
		ImportExport bean = ImportExport.newInstance();
		bean.setModuleName(null);
		bean.setDocumentName(null);
		assertFalse(bean.isContextSet());
		assertTrue(bean.isNotContextSet());
	}

	@Test
	void isContextSetTrueWhenBothSet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setModuleName("admin");
		bean.setDocumentName("User");
		assertTrue(bean.isContextSet());
		assertFalse(bean.isNotContextSet());
	}

	@Test
	void isFileExistsTrueWhenImportModeAndPathSet() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.importData);
		bean.setImportFileAbsolutePath("/tmp/file.csv");
		assertTrue(bean.isFileExists());
		assertFalse(bean.isNotFileExists());
	}

	@Test
	void isFileExistsFalseWhenExportMode() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.exportData);
		assertFalse(bean.isFileExists());
	}

	@Test
	void isShowExportTrueWhenExportMode() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.exportData);
		assertTrue(bean.isShowExport());
		assertFalse(bean.isNotShowExport());
	}

	@Test
	void isShowExportFalseWhenImportMode() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.importData);
		assertFalse(bean.isShowExport());
		assertTrue(bean.isNotShowExport());
	}

	@Test
	void isLoadTypeCreateFindTrueWhenImportAndCreateLoadType() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.importData);
		bean.setLoadType(modules.admin.ImportExport.ImportExportUtil.CREATE_RELATED_RECORDS_IF_THEY_DON_T_EXIST);
		assertTrue(bean.isLoadTypeCreateFind());
		assertFalse(bean.isNotLoadTypeCreateFind());
	}

	@Test
	void isLoadTypeCreateFindFalseWhenExportMode() {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.exportData);
		bean.setLoadType(modules.admin.ImportExport.ImportExportUtil.CREATE_RELATED_RECORDS_IF_THEY_DON_T_EXIST);
		assertFalse(bean.isLoadTypeCreateFind());
		assertTrue(bean.isNotLoadTypeCreateFind());
	}

	@Test
	void rollbackErrorsFromLocalisedDescriptionNonNull() {
		assertNotNull(RollbackErrors.fromLocalisedDescription(
				RollbackErrors.rollbackErrors.toLocalisedDescription()));
		assertNotNull(RollbackErrors.toDomainValues());
	}

	@Test
	void rollbackErrorsToCodeAndToDomainValue() {
		assertNotNull(RollbackErrors.rollbackErrors.toCode());
		assertNotNull(RollbackErrors.rollbackErrors.toDomainValue());
	}

	@Test
	void modeToCodeAndToDomainValue() {
		assertNotNull(Mode.importData.toCode());
		assertNotNull(Mode.importData.toDomainValue());
	}
}
