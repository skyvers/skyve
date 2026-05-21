package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ReportTemplate.Mode;
import modules.admin.domain.ReportTemplate.OutputFormat;
import modules.admin.domain.ReportTemplate.ReportType;
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
}
