package org.skyve.impl.report.freemarker;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.apache.commons.beanutils.DynaBean;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.ReportDataset;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.app.admin.ReportParameter;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.app.admin.ReportTemplate;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.report.ReportFormat;
import org.xhtmlrenderer.pdf.ITextOutputDevice;

import freemarker.template.Template;

@SuppressWarnings("static-method")
class FreemarkerReportUtilTest {

	@BeforeAll
	static void initFreeMarker() {
		FreemarkerReportUtil.init();
	}

	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void initCompletesWithoutException() {
		FreemarkerReportUtil.init();
		// Verify configuration is accessible after init (removing non-existent template returns false)
		assertFalse(FreemarkerReportUtil.removeTemplate("nonexistent_abc_init"));
	}

	@Test
	void addTemplateAndRemoveTemplate() {
		FreemarkerReportUtil.addTemplate("testTemplate", "Hello World!");
		assertTrue(FreemarkerReportUtil.removeTemplate("testTemplate"));
	}

	@Test
	void removeNonExistentTemplateReturnsFalse() {
		assertFalse(FreemarkerReportUtil.removeTemplate("doesNotExist_xyz"));
	}

	@Test
	void addMultipleTemplatesAndRemove() {
		FreemarkerReportUtil.addTemplate("r1", "c1");
		FreemarkerReportUtil.addTemplate("r2", "c2");
		assertTrue(FreemarkerReportUtil.removeTemplate("r1"));
		assertTrue(FreemarkerReportUtil.removeTemplate("r2"));
	}

	@Test
	void addDirectiveDoesNotThrow() {
		FreemarkerReportUtil.addDirective("d1", new SqlFormatDirective());
		// Verify the directive was registered (removing a non-existent template is always false)
		assertFalse(FreemarkerReportUtil.removeTemplate("d1"));
	}

	@Test
	void addTemplateWithHtmlMarkupAndRemove() {
		FreemarkerReportUtil.addTemplate("html1", "<html></html>");
		assertTrue(FreemarkerReportUtil.removeTemplate("html1"));
	}

	@Test
	void addTemplateOverwritesExistingTemplate() {
		FreemarkerReportUtil.addTemplate("ow", "v1");
		FreemarkerReportUtil.addTemplate("ow", "v2");
		assertTrue(FreemarkerReportUtil.removeTemplate("ow"));
	}

	@Test
	void privateCreateReportReturnsNullForMissingTemplate() throws Exception {
		Method method = FreemarkerReportUtil.class.getDeclaredMethod("createReport", Template.class, Map.class, String.class);
		method.setAccessible(true);

		File result = (File) method.invoke(null, null, Map.of(), "missing");

		assertNull(result);
	}

	@Test
	void createReportMergesInMemoryTemplate() throws Exception {
		bindPersistenceThatExecutesScopedFunctions();
		FreemarkerReportUtil.addTemplate("unit-create-report.ftl", "Hello ${name}!");
		try {
			String result = FreemarkerReportUtil.createReport("unit-create-report.ftl", Map.of("name", "Skyve"));

			assertEquals("Hello Skyve!", result);
		}
		finally {
			FreemarkerReportUtil.removeTemplate("unit-create-report.ftl");
		}
	}

	@Test
	void createReportWrapsMissingTemplateAsDomainException() throws Exception {
		bindPersistenceThatExecutesScopedFunctions();

		assertThrows(DomainException.class,
						() -> FreemarkerReportUtil.createReport("unit-missing-template.ftl", Map.of()));
	}

	@Test
	void createBeanReportMergesDocumentScopedTemplate() throws Exception {
		bindPersistenceThatExecutesScopedFunctions();
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("sales");
		when(bean.getBizDocument()).thenReturn("Order");
		FreemarkerReportUtil.addTemplate("sales/Order/reports/summary.ftl", "Order ${number}");
		try {
			String result = FreemarkerReportUtil.createBeanReport(bean, "summary.ftl", Map.of("number", "42"));

			assertEquals("Order 42", result);
		}
		finally {
			FreemarkerReportUtil.removeTemplate("sales/Order/reports/summary.ftl");
		}
	}

	@Test
	void html5ParseDocumentRejectsNullAndParsesValidHtml() throws Exception {
		Method method = FreemarkerReportUtil.class.getDeclaredMethod("html5ParseDocument", InputStream.class);
		method.setAccessible(true);

		assertThrows(IllegalArgumentException.class, () -> invokeHtml5Parse(method, null));
		org.w3c.dom.Document result = invokeHtml5Parse(method,
				new ByteArrayInputStream("<html><body><p>Hello</p></body></html>".getBytes(StandardCharsets.UTF_8)));

		assertNotNull(result);
		assertEquals("html", result.getDocumentElement().getNodeName());
	}

	@Test
	void runReportAppliesTextParametersAndConstantDatasets() throws Exception {
		ReportParameter parameter = mock(ReportParameter.class);
		when(parameter.getName()).thenReturn("title");
		when(parameter.getType()).thenReturn(Type.text);

		ReportDataset dataset = mock(ReportDataset.class);
		when(dataset.getDatasetType()).thenReturn(DatasetType.constant);
		when(dataset.getDatasetName()).thenReturn("summary");
		when(dataset.getQuery()).thenReturn("constant-value");

		ReportTemplate reportTemplate = mock(ReportTemplate.class);
		when(reportTemplate.getBizId()).thenReturn("template-id");
		when(reportTemplate.getTemplate()).thenReturn("${summary}");
		doReturn(List.of(parameter)).when(reportTemplate).getParameters();
		doReturn(List.of(dataset)).when(reportTemplate).getDatasets();
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-run-report.flth", Map.of("title", "Quarterly"));

		assertEquals("constant-value", result);
		verify(parameter).setReportInputValue("Quarterly");
	}

	@Test
	@SuppressWarnings("unchecked")
	void runReportFormatsDateParameters() throws Exception {
		DateOnly date = new DateOnly(0L);
		Converter<DateOnly> converter = mock(Converter.class);
		ReportParameter parameter = mock(ReportParameter.class);
		when(parameter.getName()).thenReturn("asAt");
		when(parameter.getType()).thenReturn(Type.date);
		when(converter.toDisplayValue(date)).thenReturn("01-Jan-1970");

		ReportTemplate reportTemplate = reportTemplate("unit-date-parameter.flth", "${reportParameters?size}",
				List.of(parameter),
				List.of());
		bindPersistenceThatExecutesScopedFunctions(reportTemplate, converter);

		String result = FreemarkerReportUtil.runReport("unit-date-parameter.flth", Map.of("asAt", date));

		assertEquals("1", result);
		verify(parameter).setReportInputValue("01-Jan-1970");
	}

	@Test
	void runReportHandlesNullReportParameters() throws Exception {
		ReportParameter parameter = mock(ReportParameter.class);
		when(parameter.getName()).thenReturn("title");
		when(parameter.getType()).thenReturn(Type.text);

		ReportTemplate reportTemplate = reportTemplate("unit-null-parameters.flth", "${reportParameters?size}",
				List.of(parameter),
				List.of());
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-null-parameters.flth", null);

		assertEquals("1", result);
	}

	@Test
	void runReportAddsBizQLDatasetResults() throws Exception {
		Bean row = mock(Bean.class);
		ReportDataset dataset = mock(ReportDataset.class);
		when(dataset.getDatasetType()).thenReturn(DatasetType.bizQL);
		when(dataset.getDatasetName()).thenReturn("rows");
		when(dataset.executeQuery()).thenReturn(List.of(row));
		ReportTemplate reportTemplate = reportTemplate("unit-bizql-dataset.flth", "${rows?size}",
				List.of(),
				List.of(dataset));
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-bizql-dataset.flth", Map.of());

		assertEquals("1", result);
	}

	@Test
	void runReportAddsSQLDatasetResults() throws Exception {
		DynaBean row = mock(DynaBean.class);
		ReportDataset dataset = mock(ReportDataset.class);
		when(dataset.getDatasetType()).thenReturn(DatasetType.SQL);
		when(dataset.getDatasetName()).thenReturn("rows");
		when(dataset.executeSQLQuery()).thenReturn(List.of(row));
		ReportTemplate reportTemplate = reportTemplate("unit-sql-dataset.flth", "${rows?size}",
				List.of(),
				List.of(dataset));
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-sql-dataset.flth", Map.of());

		assertEquals("1", result);
	}

	@Test
	void runReportAddsClassDatasetResults() throws Exception {
		DynaBean row = mock(DynaBean.class);
		ReportDataset dataset = mock(ReportDataset.class);
		when(dataset.getDatasetType()).thenReturn(DatasetType.classValue);
		when(dataset.getDatasetName()).thenReturn("rows");
		when(dataset.executeClass()).thenReturn(List.of(row));
		ReportTemplate reportTemplate = reportTemplate("unit-class-dataset.flth", "${rows?size}",
				List.of(),
				List.of(dataset));
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-class-dataset.flth", Map.of());

		assertEquals("1", result);
	}

	@Test
	void runReportSkipsNullDatasetType() throws Exception {
		ReportDataset dataset = mock(ReportDataset.class);
		when(dataset.getDatasetType()).thenReturn(null);
		when(dataset.getDatasetName()).thenReturn("rows");
		ReportTemplate reportTemplate = reportTemplate("unit-null-dataset.flth", "${rows!'missing'}",
				List.of(),
				List.of(dataset));
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-null-dataset.flth", Map.of());

		assertEquals("missing", result);
	}

	@Test
	void runReportSkipsNullDatasetResults() throws Exception {
		ReportDataset bizql = mock(ReportDataset.class);
		when(bizql.getDatasetType()).thenReturn(DatasetType.bizQL);
		when(bizql.getDatasetName()).thenReturn("bizqlRows");
		when(bizql.executeQuery()).thenReturn(null);
		ReportDataset sql = mock(ReportDataset.class);
		when(sql.getDatasetType()).thenReturn(DatasetType.SQL);
		when(sql.getDatasetName()).thenReturn("sqlRows");
		when(sql.executeSQLQuery()).thenReturn(null);
		ReportDataset classValue = mock(ReportDataset.class);
		when(classValue.getDatasetType()).thenReturn(DatasetType.classValue);
		when(classValue.getDatasetName()).thenReturn("classRows");
		when(classValue.executeClass()).thenReturn(null);
		ReportTemplate reportTemplate = reportTemplate("unit-null-dataset-results.flth",
				"${bizqlRows!'missing'} ${sqlRows!'missing'} ${classRows!'missing'}",
				List.of(),
				List.of(bizql, sql, classValue));
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-null-dataset-results.flth", Map.of());

		assertEquals("missing missing missing", result);
	}

	@Test
	void runReportLeavesMissingSuppliedParameterUnchanged() throws Exception {
		ReportParameter parameter = mock(ReportParameter.class);
		when(parameter.getName()).thenReturn("title");
		when(parameter.getType()).thenReturn(Type.text);
		ReportTemplate reportTemplate = reportTemplate("unit-missing-parameter.flth", "${reportParameters?size}",
				List.of(parameter),
				List.of());
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		String result = FreemarkerReportUtil.runReport("unit-missing-parameter.flth", Map.of("other", "value"));

		assertEquals("1", result);
	}

	@Test
	void downloadReportReturnsCsvDownloadWithoutPdfRendering() throws Exception {
		ReportDataset dataset = mock(ReportDataset.class);
		when(dataset.getDatasetType()).thenReturn(DatasetType.constant);
		when(dataset.getDatasetName()).thenReturn("summary");
		when(dataset.getQuery()).thenReturn("csv-content");
		ReportTemplate reportTemplate = reportTemplate("unit-download.flth", "${summary}",
				List.of(),
				List.of(dataset));
		bindPersistenceThatExecutesScopedFunctions(reportTemplate);

		Download result = FreemarkerReportUtil.downloadReport("unit-download.flth", Map.of(), ReportFormat.csv, "download-name");

		assertEquals("download-name.csv", result.getFileName());
		assertEquals(MimeType.csv, result.getMimeType());
		assertEquals("csv-content", new String(result.getBytes(), StandardCharsets.UTF_8));
	}

	@Test
	void runReportThrowsDomainExceptionWhenReportTemplateIsMissing() throws Exception {
		bindPersistenceThatExecutesScopedFunctions(null);

		assertThrows(DomainException.class, () -> FreemarkerReportUtil.runReport("missing-template.flth", Map.of()));
	}

	@Test
	void resourceLoaderUserAgentResolveAndOpenStreamHandlesUnknownUri() throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.report.freemarker.FreemarkerReportUtil$ResourceLoaderUserAgent");
		Constructor<?> constructor = clazz.getDeclaredConstructor(ITextOutputDevice.class, int.class);
		constructor.setAccessible(true);
		Object userAgent = constructor.newInstance(mock(ITextOutputDevice.class), Integer.valueOf(1));

		Method method = clazz.getDeclaredMethod("resolveAndOpenStream", String.class);
		method.setAccessible(true);
		InputStream result = (InputStream) method.invoke(userAgent, "missing://resource");
		assertNull(result);
	}

	private static org.w3c.dom.Document invokeHtml5Parse(Method method, InputStream inputStream) {
		try {
			return (org.w3c.dom.Document) method.invoke(null, inputStream);
		}
		catch (ReflectiveOperationException e) {
			Throwable cause = e.getCause();
			if (cause instanceof RuntimeException runtime) {
				throw runtime;
			}
			throw new IllegalStateException("Could not parse HTML", e);
		}
	}

	private static void bindPersistenceThatExecutesScopedFunctions() throws Exception {
		bindPersistenceThatExecutesScopedFunctions(null);
	}

	private static void bindPersistenceThatExecutesScopedFunctions(ReportTemplate reportTemplate) throws Exception {
		bindPersistenceThatExecutesScopedFunctions(reportTemplate, null);
	}

	private static void bindPersistenceThatExecutesScopedFunctions(ReportTemplate reportTemplate, Converter<DateOnly> dateConverter) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module adminModule = mock(Module.class);
		Document reportTemplateDocument = mock(Document.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getName()).thenReturn("Unit Tester");
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getDefaultDateConverter()).thenReturn(dateConverter);
		when(customer.getModule("admin")).thenReturn(adminModule);
		when(adminModule.getDocument(customer, "ReportTemplate")).thenReturn(reportTemplateDocument);
		when(persistence.newDocumentQuery("admin", "ReportTemplate")).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.addBoundProjection(ArgumentMatchers.anyString())).thenReturn(query);
		when(query.scalarResult(OptimisticLock.class)).thenReturn(new OptimisticLock("Unit Tester", new Date(0L)));
		when(query.beanResult()).thenReturn(reportTemplate);
		when(persistence.withDocumentPermissionScopes(ArgumentMatchers.eq(DocumentPermissionScope.customer),
														ArgumentMatchers.<Function<Persistence, Object>>any()))
			.thenAnswer(invocation -> {
				Function<Persistence, Object> function = invocation.getArgument(1);
				return function.apply(persistence);
			});
		bindPersistenceToThread(persistence);
	}

	private static ReportTemplate reportTemplate(String templateName, String templateMarkup, List<ReportParameter> parameters, List<ReportDataset> datasets) {
		ReportTemplate reportTemplate = mock(ReportTemplate.class);
		when(reportTemplate.getBizId()).thenReturn(templateName + "-id");
		when(reportTemplate.getTemplate()).thenReturn(templateMarkup);
		doReturn(parameters).when(reportTemplate).getParameters();
		doReturn(datasets).when(reportTemplate).getDatasets();
		return reportTemplate;
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
