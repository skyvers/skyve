package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

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
	void dataBuilderPopulatesImportExportBean() throws Exception {
		ImportExport bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ImportExport.MODULE_NAME, ImportExport.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		assertEquals(ImportExport.MODULE_NAME, bean.getBizModule());
		assertEquals(ImportExport.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void modeSetAndGet() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.importData);
		assertEquals(Mode.importData, bean.getMode());
	}

	@Test
	void modeExportSetAndGet() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		bean.setMode(Mode.exportData);
		assertEquals(Mode.exportData, bean.getMode());
	}

	@Test
	void rollbackErrorsSetAndGet() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		bean.setRollbackErrors(RollbackErrors.rollbackErrors);
		assertEquals(RollbackErrors.rollbackErrors, bean.getRollbackErrors());
	}

	@Test
	void moduleAndDocumentNameSetAndGet() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		bean.setModuleName("admin");
		bean.setDocumentName("User");
		assertEquals("admin", bean.getModuleName());
		assertEquals("User", bean.getDocumentName());
	}

	@Test
	void filePathsSetAndGet() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		bean.setImportFileName("data.csv");
		bean.setImportFileAbsolutePath("/tmp/data.csv");
		bean.setExportFileAbsolutePath("/tmp/export.xlsx");
		assertEquals("data.csv", bean.getImportFileName());
		assertEquals("/tmp/data.csv", bean.getImportFileAbsolutePath());
		assertEquals("/tmp/export.xlsx", bean.getExportFileAbsolutePath());
	}

	@Test
	void booleanFlagsSetAndGet() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		bean.setColumnTitlesOnly(Boolean.TRUE);
		bean.setAdvancedMode(Boolean.FALSE);
		assertEquals(Boolean.TRUE, bean.getColumnTitlesOnly());
		assertEquals(Boolean.FALSE, bean.getAdvancedMode());
	}

	@Test
	void resultsSetAndGet() throws Exception {
		ImportExport bean = ImportExport.newInstance();
		bean.setResults("Imported: 10 records");
		assertEquals("Imported: 10 records", bean.getResults());
	}
}
