package modules.admin.ReportManager.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.util.JSON;
import org.skyve.web.WebContext;

import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportManager.ReportManagerExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportDataset;
import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

/**
 * Tests import report specification error paths and private load handling.
 */
@SuppressWarnings("static-method")
class ImportReportSpecificationsTest extends AbstractH2Test {
	@Test
	void uploadWithUnsupportedExtensionThrowsValidationException() {
		ImportReportSpecifications action = new ImportReportSpecifications();
		Upload upload = upload("report.txt", MimeType.plain, "not a report");
		ReportManagerExtension bean = new ReportManagerExtension();
		UploadException uploadException = new UploadException();

		ValidationException exception = assertThrows(ValidationException.class,
				() -> action.upload(bean, upload, uploadException, null));

		assertEquals("File type text/plain is not recognised as either json or zip",
				exception.getMessages().get(0).getText());
	}

	@Test
	void uploadWithMalformedJsonThrowsRecognisedReportValidationException() {
		ImportReportSpecifications action = new ImportReportSpecifications();
		Upload upload = upload("report.json", MimeType.json, "{not-json");
		ReportManagerExtension bean = new ReportManagerExtension();
		UploadException uploadException = new UploadException();

		ValidationException exception = assertThrows(ValidationException.class,
				() -> action.upload(bean, upload, uploadException, null));

		assertEquals("The file was not recognised as a valid report", exception.getMessages().get(0).getText());
	}

	@Test
	void uploadWithValidJsonAndValidateOnlyReportsSuccessGrowl() throws Exception {
		ImportReportSpecifications action = new ImportReportSpecifications();
		ReportTemplateExtension template = validTemplate("Valid JSON Report");
		Upload upload = upload("report.json", MimeType.json, JSON.marshall(CORE.getCustomer(), template));
		ReportManagerExtension bean = new ReportManagerExtension();
		WebContext webContext = mock(WebContext.class);

		ReportManagerExtension result = action.upload(bean, upload, new UploadException(), webContext);

		assertEquals(bean, result);
		verify(webContext).growl(MessageSeverity.info,
				"Report validated ok - select import option to import this report");
	}

	@Test
	void loadReportClearsSchedulesAndSavesTemplate() throws Exception {
		ImportReportSpecifications action = new ImportReportSpecifications();
		ReportTemplateExtension template = ReportTemplate.newInstance();
		template.setName("Imported Report");
		template.setScheduled(Boolean.TRUE);
		template.setCronExpression("0 0 * * * ?");

		invokeLoadReport(action, new ReportManagerExtension(), template);

		assertFalse(template.getScheduled().booleanValue());
		assertNull(template.getCronExpression());
	}

	@Test
	void validateReportAddsTemplateNameToReplacementList() throws Exception {
		ReportTemplateExtension template = validTemplate("Replacement Candidate");
		List<String> templatesToReplace = new ArrayList<>();

		invokeValidateReport(template, false, templatesToReplace);

		assertEquals(List.of("Replacement Candidate"), templatesToReplace);
	}

	@Test
	void validateReportRejectsSqlDatasetForMultiTenantConfiguration() {
		String savedCustomer = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = null;
			ReportTemplateExtension template = validTemplate("SQL Template");
			ReportDatasetExtension dataset = ReportDataset.newInstance();
			dataset.setDatasetName("rows");
			dataset.setDatasetType(DatasetType.SQL);
			dataset.setQuery("select 1");
			template.addDatasetsElement(dataset);
			ArrayList<String> templatesToReplace = new ArrayList<>();

			ValidationException exception = assertThrows(ValidationException.class,
					() -> invokeValidateReport(template, false, templatesToReplace));

			assertTrue(exception.getMessages().get(0).getText().contains("SQL dataset in report template SQL Template"));
		}
		finally {
			UtilImpl.CUSTOMER = savedCustomer;
		}
	}

	@SuppressWarnings("resource")
	private static Upload upload(String fileName, MimeType mimeType, String contents) {
		return new Upload(fileName,
				new WebFileInputStream(new ByteArrayInputStream(contents.getBytes(java.nio.charset.StandardCharsets.UTF_8))),
				mimeType);
	}

	private static ReportTemplateExtension validTemplate(String name) {
		ReportTemplateExtension template = ReportTemplate.newInstance();
		template.setName(name);
		template.setTemplateName(name + ".html");
		return template;
	}

	private static void invokeLoadReport(ImportReportSpecifications action,
			ReportManagerExtension bean,
			PersistentBean report)
			throws Exception {
		Method method = ImportReportSpecifications.class.getDeclaredMethod("loadReport", ReportManagerExtension.class,
				PersistentBean.class);
		method.setAccessible(true);
		try {
			method.invoke(action, bean, report);
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static void invokeValidateReport(PersistentBean report, boolean withRemove, List<String> templatesToReplace)
			throws Exception {
		Method method = ImportReportSpecifications.class.getDeclaredMethod("validateReport", PersistentBean.class,
				boolean.class, List.class);
		method.setAccessible(true);
		try {
			method.invoke(null, report, Boolean.valueOf(withRemove), templatesToReplace);
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}
}
