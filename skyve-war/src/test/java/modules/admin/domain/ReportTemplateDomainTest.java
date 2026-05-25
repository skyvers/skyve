package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ReportTemplate.GenerateExisting;
import modules.admin.domain.ReportTemplate.Mode;
import modules.admin.domain.ReportTemplate.OutputFormat;
import modules.admin.domain.ReportTemplate.ReportType;
import modules.admin.domain.ReportTemplate.WizardState;
import util.AbstractH2Test;

/**
 * Tests for the {@link ReportTemplate} admin domain bean (persistent, abstract).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class ReportTemplateDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesReportTemplateBean() throws Exception {
		ReportTemplate bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		assertEquals(ReportTemplate.MODULE_NAME, bean.getBizModule());
		assertEquals(ReportTemplate.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void nameAndDescriptionSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setName("Monthly Sales Report");
		bean.setDescription("Report of monthly sales figures");
		assertEquals("Monthly Sales Report", bean.getName());
		assertEquals("Report of monthly sales figures", bean.getDescription());
	}

	@Test
	void reportTypeSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setReportType(ReportType.freemarker);
		assertEquals(ReportType.freemarker, bean.getReportType());
		bean.setReportType(ReportType.jasper);
		assertEquals(ReportType.jasper, bean.getReportType());
	}

	@Test
	void outputFormatSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setOutputFormat(OutputFormat.PDF);
		assertEquals(OutputFormat.PDF, bean.getOutputFormat());
		bean.setOutputFormat(OutputFormat.CSV);
		assertEquals(OutputFormat.CSV, bean.getOutputFormat());
	}

	@Test
	void modeSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setMode(Mode.SQL);
		assertEquals(Mode.SQL, bean.getMode());
		bean.setMode(Mode.bean);
		assertEquals(Mode.bean, bean.getMode());
	}

	@Test
	void moduleAndDocumentNameSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setModuleName("admin");
		bean.setDocumentName("User");
		assertEquals("admin", bean.getModuleName());
		assertEquals("User", bean.getDocumentName());
	}

	@Test
	void reportNameAndTemplateSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setReportName("salesReport.jrxml");
		bean.setTemplateName("salesTemplate.ftl");
		assertEquals("salesReport.jrxml", bean.getReportName());
		assertEquals("salesTemplate.ftl", bean.getTemplateName());
	}

	@Test
	void cronExpressionSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setCronExpression("0 0 * * * ?");
		assertEquals("0 0 * * * ?", bean.getCronExpression());
	}

	@Test
	void startAndEndTimeSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		DateTime start = new DateTime();
		DateTime end = new DateTime();
		bean.setStartTime(start);
		bean.setEndTime(end);
		assertNotNull(bean.getStartTime());
		assertNotNull(bean.getEndTime());
	}

	@Test
	void flagsSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setEnabled(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getEnabled());
	}

	@Test
	void restrictToRoleSetAndGet() throws Exception {
		ReportTemplate bean = ReportTemplate.newInstance();
		bean.setRestrictToRole("admin.BasicUser");
		assertEquals("admin.BasicUser", bean.getRestrictToRole());
	}

        @Test
        @SuppressWarnings("static-method")
        void reportTypeFromCodeAndToDomainValues() {
                assertEquals(ReportType.jasper, ReportType.fromCode("Jasper"));
                assertNull(ReportType.fromCode("notexist"));
                assertNull(ReportType.fromLocalisedDescription("notexist"));
                assertNotNull(ReportType.fromLocalisedDescription(ReportType.jasper.toLocalisedDescription()));
                assertNotNull(ReportType.toDomainValues());
                assertEquals(2, ReportType.toDomainValues().size());
        }

        @Test
        @SuppressWarnings("static-method")
        void outputFormatFromCodeAndToDomainValues() {
                assertEquals(OutputFormat.CSV, OutputFormat.fromCode("CSV"));
                assertNull(OutputFormat.fromCode("notexist"));
                assertNull(OutputFormat.fromLocalisedDescription("notexist"));
                assertNotNull(OutputFormat.fromLocalisedDescription(OutputFormat.CSV.toLocalisedDescription()));
                assertNotNull(OutputFormat.toDomainValues());
                assertEquals(2, OutputFormat.toDomainValues().size());
        }

        @Test
        @SuppressWarnings("static-method")
        void modeFromCodeAndToDomainValues() {
                assertEquals(Mode.SQL, Mode.fromCode("sql"));
                assertNull(Mode.fromCode("notexist"));
                assertNull(Mode.fromLocalisedDescription("notexist"));
                assertNotNull(Mode.fromLocalisedDescription(Mode.SQL.toLocalisedDescription()));
                assertNotNull(Mode.toDomainValues());
                assertEquals(2, Mode.toDomainValues().size());
        }

        @Test
        @SuppressWarnings("static-method")
        void reportTypeToCodeAndToDomainValue() {
                assertEquals("Jasper", ReportType.jasper.toCode());
                assertNotNull(ReportType.jasper.toDomainValue());
        }

        @Test
        @SuppressWarnings("static-method")
        void outputFormatToCodeAndToDomainValue() {
                assertEquals("CSV", OutputFormat.CSV.toCode());
                assertNotNull(OutputFormat.CSV.toDomainValue());
        }

        @Test
        @SuppressWarnings("static-method")
        void modeToCodeAndToDomainValue() {
                assertEquals("sql", Mode.SQL.toCode());
                assertNotNull(Mode.SQL.toDomainValue());
        }

	@Test
	@SuppressWarnings("static-method")
	void wizardStateFromCodeAndFromLocalisedDescription() {
		assertEquals(WizardState.enterDetails, WizardState.fromCode("enterDetails"));
		assertNull(WizardState.fromCode("nonexistent"));
		assertNotNull(WizardState.fromLocalisedDescription(WizardState.enterDetails.toLocalisedDescription()));
		assertNull(WizardState.fromLocalisedDescription("nonexistent"));
		assertNotNull(WizardState.toDomainValues());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateExistingFromCodeAndFromLocalisedDescription() {
		assertEquals(GenerateExisting.generate, GenerateExisting.fromCode("Generate"));
		assertNull(GenerateExisting.fromCode("nonexistent"));
		assertNotNull(GenerateExisting.fromLocalisedDescription(GenerateExisting.generate.toLocalisedDescription()));
		assertNull(GenerateExisting.fromLocalisedDescription("nonexistent"));
		assertNotNull(GenerateExisting.toDomainValues());
	}
}
