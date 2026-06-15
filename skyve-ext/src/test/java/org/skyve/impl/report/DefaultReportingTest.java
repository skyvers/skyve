package org.skyve.impl.report;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.ReportDataset;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.app.admin.ReportTemplate;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;

import freemarker.template.TemplateNotFoundException;

@SuppressWarnings({"static-method", "unchecked"})
class DefaultReportingTest {
	@TempDir
	File tempDir;

	@BeforeEach
	void beforeEach() {
		DefaultReporting.get().startup();
	}

	@AfterEach
	void afterEach() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void getReturnsSingletonAndLifecycleMethodsComplete() {
		DefaultReporting reporting = DefaultReporting.get();

		assertSame(reporting, DefaultReporting.get());
		assertDoesNotThrow(reporting::startup);
		assertDoesNotThrow(reporting::shutdown);
	}

	@Test
	void multiReportAttachmentRequiresAtLeastOneReport() {
		DefaultReporting reporting = DefaultReporting.get();

		assertThrows(IllegalArgumentException.class, () -> reporting.getMailAttachmentFromJasperReport(List.of()));
	}

	@Test
	void addTemplateFacadeAcceptsInMemoryTemplate() {
		DefaultReporting reporting = DefaultReporting.get();

		assertDoesNotThrow(() -> reporting.addTemplate("default-reporting-template.flth", "Hello ${name}"));
	}

	@Test
	void createFreemarkerReportUsesInMemoryTemplate() throws Exception {
		DefaultReporting reporting = DefaultReporting.get();
		bindPersistenceThatExecutesScopedFunctions();
		String templateName = "default-reporting-create-template.flth";
		reporting.addTemplate(templateName, "Hello ${name}");

		String result = reporting.createFreemarkerReport(templateName, Map.of("name", "Skyve"));

		assertEquals("Hello Skyve", result);
		assertNotNull(reporting.getFreemarkerTemplate(templateName));
	}

	@Test
	void createFreemarkerBeanReportUsesDocumentScopedTemplate() throws Exception {
		DefaultReporting reporting = DefaultReporting.get();
		bindPersistenceThatExecutesScopedFunctions();
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("sales");
		when(bean.getBizDocument()).thenReturn("Order");
		reporting.addTemplate("sales/Order/reports/default-reporting-bean.ftl", "Order ${number}");

		String result = reporting.createFreemarkerBeanReport(bean, "default-reporting-bean.ftl", Map.of("number", "42"));

		assertEquals("Order 42", result);
		assertNotNull(reporting.getBeanReport(bean, "default-reporting-bean.ftl"));
	}

	@Test
	void runAndDownloadFreemarkerReportUseReportTemplate() throws Exception {
		DefaultReporting reporting = DefaultReporting.get();
		ReportDataset dataset = mock(ReportDataset.class);
		when(dataset.getDatasetType()).thenReturn(DatasetType.constant);
		when(dataset.getDatasetName()).thenReturn("summary");
		when(dataset.getQuery()).thenReturn("csv-content");
		ReportTemplate reportTemplate = mock(ReportTemplate.class);
		when(reportTemplate.getBizId()).thenReturn("default-reporting-run-id");
		when(reportTemplate.getTemplate()).thenReturn("${summary}");
		doReturn(List.of()).when(reportTemplate).getParameters();
		doReturn(List.of(dataset)).when(reportTemplate).getDatasets();
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String rendered = reporting.runFreemarkerReport("default-reporting-run.flth", Map.of());
		Download download = reporting.downloadFreemarkerReport("default-reporting-run.flth", Map.of(), ReportFormat.csv, "default-report");

		assertEquals("csv-content", rendered);
		assertEquals("default-report.csv", download.getFileName());
		assertEquals(MimeType.csv, download.getMimeType());
		assertEquals("csv-content", new String(download.getBytes(), StandardCharsets.UTF_8));
	}

	@Test
	void freemarkerPdfFacadeMethodsPropagateMissingTemplateErrors() throws Exception {
		DefaultReporting reporting = DefaultReporting.get();
		bindPersistenceThatExecutesScopedFunctions();
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("sales");
		when(bean.getBizDocument()).thenReturn("Order");
		File output = new File("target/default-reporting-missing.pdf");

		assertThrows(DomainException.class,
				() -> reporting.createFreemarkerReportPDF("default-reporting-missing.ftl", Map.of(), "default-reporting-missing"));
		assertThrows(TemplateNotFoundException.class,
				() -> reporting.createFreemarkerBeanReportPDF(bean, "default-reporting-missing.ftl", Map.of(), "default-reporting-missing"));
			assertThrows(NullPointerException.class,
					() -> reporting.generateFreemarkerPDFFromHTML((String) null, output));
			ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
			assertThrows(NullPointerException.class,
						() -> reporting.generateFreemarkerPDFFromHTML((String) null, outputStream));
		}

	@Test
	void freemarkerPdfFacadeMethodsGeneratePdfOutput() throws Exception {
		DefaultReporting reporting = DefaultReporting.get();
		String html = "<html><body><p>default-reporting-pdf</p></body></html>";
		File streamFile = new File(tempDir, "stream.pdf");
		File stringFile = new File(tempDir, "string.pdf");
		File urlFile = new File(tempDir, "url.pdf");
		File htmlFile = new File(tempDir, "source.html");
		java.nio.file.Files.writeString(htmlFile.toPath(), html, StandardCharsets.UTF_8);
		ByteArrayOutputStream stringOut = new ByteArrayOutputStream();

		reporting.generateFreemarkerPDFFromHTML(
				new java.io.ByteArrayInputStream(html.getBytes(StandardCharsets.UTF_8)),
				streamFile);
		reporting.generateFreemarkerPDFFromHTML(html, stringFile);
		reporting.generateFreemarkerPDFFromHTML(html, stringOut);
		reporting.generateFreemarkerPDFFromHTMLURL(htmlFile.toURI().toURL().toString(), urlFile);

		assertTrue(streamFile.length() > 0);
		assertTrue(stringFile.length() > 0);
		assertTrue(stringOut.size() > 0);
		assertTrue(urlFile.length() > 0);
	}

	private static void bindPersistenceThatExecutesScopedFunctions() throws Exception {
		bindPersistenceThatExecutesScopedFunctions(null);
	}

	private static void bindPersistenceThatExecutesScopedFunctions(ReportTemplate reportTemplate) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.REPORT_TEMPLATE_DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.beanResult()).thenReturn(reportTemplate);
		when(persistence.withDocumentPermissionScopes(any(DocumentPermissionScope.class), any(Function.class)))
				.thenAnswer(invocation -> {
					Function<Persistence, Object> function = invocation.getArgument(1);
					return function.apply(persistence);
				});
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
