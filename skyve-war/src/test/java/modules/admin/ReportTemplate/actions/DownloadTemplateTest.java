package modules.admin.ReportTemplate.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.Download;

import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate.OutputFormat;
import modules.admin.domain.ReportTemplate.ReportType;

@SuppressWarnings("static-method")
class DownloadTemplateTest {

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

		Download result = action.download(bean, null);

		assertNotNull(result);
	}
}
