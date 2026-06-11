package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ReportManager.ImportActionType;
import modules.test.AbstractSkyveTest;

class ReportManagerDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesReportManager() {
		ReportManager bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ReportManager.MODULE_NAME, ReportManager.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void newInstanceCreatesReportManager() {
		ReportManager bean = ReportManager.newInstance();
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() {
		ReportManager bean = ReportManager.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("ReportManager", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void pathToZipSetAndGet() {
		ReportManager bean = ReportManager.newInstance();
		bean.setPathToZip("/tmp/reports.zip");
		assertEquals("/tmp/reports.zip", bean.getPathToZip());
	}

	@Test
	@SuppressWarnings("static-method")
	void importActionTypeValidate() {
		ReportManager bean = ReportManager.newInstance();
		bean.setImportActionType(ImportActionType.validateOnlyReportConfigurationsAndTemplates);
		assertEquals(ImportActionType.validateOnlyReportConfigurationsAndTemplates, bean.getImportActionType());
	}

	@Test
	@SuppressWarnings("static-method")
	void importActionTypeImport() {
		ReportManager bean = ReportManager.newInstance();
		bean.setImportActionType(ImportActionType.validateThenImportReportConfigurationsAndTemplates);
		assertEquals(ImportActionType.validateThenImportReportConfigurationsAndTemplates, bean.getImportActionType());
	}

	@Test
	@SuppressWarnings("static-method")
	void currentReportsListIsNotNull() {
		ReportManager bean = ReportManager.newInstance();
		assertNotNull(bean.getCurrentReports());
	}

	@Test
	@SuppressWarnings("static-method")
	void importActionTypeFromCodeAndFromLocalisedDescription() {
		assertEquals(ImportActionType.validateOnlyReportConfigurationsAndTemplates,
				ImportActionType.fromCode("validate"));
		assertNull(ImportActionType.fromCode("nonexistent"));
		assertNotNull(ImportActionType.fromLocalisedDescription(
				ImportActionType.validateOnlyReportConfigurationsAndTemplates.toLocalisedDescription()));
		assertNull(ImportActionType.fromLocalisedDescription("nonexistent"));
		assertNotNull(ImportActionType.toDomainValues());
	}
}
