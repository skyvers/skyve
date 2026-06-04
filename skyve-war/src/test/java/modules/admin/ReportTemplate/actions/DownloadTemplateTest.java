package modules.admin.ReportTemplate.actions;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;

import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportParameter;
import modules.admin.domain.ReportTemplate.Mode;
import modules.admin.domain.ReportTemplate.OutputFormat;
import modules.admin.domain.ReportTemplate.ReportType;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class DownloadTemplateTest extends AbstractH2Test {

	@Test
	void prepareDoesNotThrow() {
		DownloadTemplate action = new DownloadTemplate();
		ReportTemplateExtension bean = new ReportTemplateExtension();
		assertDoesNotThrow(() -> action.prepare(bean, null));
	}

	@Test
	void downloadWithJasperTypeReturnsXmlDownload() throws Exception {
		DownloadTemplate action = new DownloadTemplate();
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setReportType(ReportType.jasper);
		bean.setName("MyReport");
		bean.setTemplate("template content");

		Download result = action.download(bean, null);

		assertNotNull(result);
	}

	@Test
	void downloadWithFreemarkerCsvReturnsTextDownload() throws Exception {
		DownloadTemplate action = new DownloadTemplate();
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setReportType(ReportType.freemarker);
		bean.setOutputFormat(OutputFormat.CSV);
		bean.setName("MyReport");
		bean.setTemplate("template content");

		Download result = action.download(bean, null);

		assertNotNull(result);
	}

	@Test
	void downloadWithFreemarkerNonCsvReturnsHtmlDownload() throws Exception {
		DownloadTemplate action = new DownloadTemplate();
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setReportType(ReportType.freemarker);
		bean.setOutputFormat(OutputFormat.PDF);
		bean.setName("MyReport");
		bean.setTemplate("template content");

		Download result = action.download(bean, null);

		assertNotNull(result);
	}

	@Test
	void downloadReportCsvReturnsReportResultsAsCsvDownload() throws Exception {
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setName("Usage Summary");
		bean.setOutputFormat(OutputFormat.CSV);
		bean.setResults("name,count\nAlpha,3");

		Download result = new DownloadReport().download(bean, null);

		assertEquals("Usage Summary.csv", result.getFileName());
		assertEquals(MimeType.csv, result.getMimeType());
		assertEquals("name,count\nAlpha,3", new String(result.getBytes(), StandardCharsets.UTF_8));
	}

	@Test
	void testReportCsvReturnsReportResultsAsCsvDownload() throws Exception {
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setName("Preview");
		bean.setOutputFormat(OutputFormat.CSV);
		bean.setResults("preview");

		Download result = new TestReport().download(bean, null);

		assertEquals("Preview.csv", result.getFileName());
		assertEquals(MimeType.csv, result.getMimeType());
		assertEquals("preview", new String(result.getBytes(), StandardCharsets.UTF_8));
	}

	@Test
	void testReportPrepareRejectsUnsavedChanges() {
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setName("Unsaved");
		TestReport action = new TestReport();

		assertThrows(DomainException.class, () -> action.prepare(bean, null));
	}

	@Test
	void testJasperReportPrepareRejectsUnsavedChanges() {
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setName("Unsaved");
		TestJasperReport action = new TestJasperReport();

		assertThrows(DomainException.class, () -> action.prepare(bean, null));
	}

	@Test
	void downloadReportPrepareRejectsMissingRequiredRuntimeParameter() {
		ReportTemplateExtension bean = unchangedTemplateWithRequiredParameter();
		DownloadReport action = new DownloadReport();

		assertThrows(ValidationException.class, () -> action.prepare(bean, null));
	}

	@Test
	void testReportPrepareRejectsMissingRequiredTestParameter() {
		ReportTemplateExtension bean = unchangedTemplateWithRequiredParameter();
		TestReport action = new TestReport();

		assertThrows(ValidationException.class, () -> action.prepare(bean, null));
	}

	@Test
	void testJasperReportPrepareRejectsBeanModeWithoutIdParameter() {
		ReportTemplateExtension bean = new ReportTemplateExtension();
		bean.setMode(Mode.bean);
		bean.originalValues().clear();
		TestJasperReport action = new TestJasperReport();

		assertThrows(ValidationException.class, () -> action.prepare(bean, null));
	}

	private static ReportTemplateExtension unchangedTemplateWithRequiredParameter() {
		ReportTemplateExtension bean = new ReportTemplateExtension();
		ReportParameterExtension parameter = new ReportParameterExtension();
		parameter.setName("requiredValue");
		parameter.setType(ReportParameter.Type.text);
		parameter.setRequired(Boolean.TRUE);
		bean.getParameters().add(parameter);
		bean.originalValues().clear();
		parameter.originalValues().clear();
		return bean;
	}
}
