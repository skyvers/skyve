package modules.admin.ReportManager.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.WebFileInputStream;

import modules.admin.ReportManager.ReportManagerExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

/**
 * Tests import report specification error paths and private load handling.
 */
@SuppressWarnings("static-method")
class ImportReportSpecificationsTest extends AbstractH2Test {
	@Test
	void uploadWithUnsupportedExtensionThrowsValidationException() throws Exception {
		ImportReportSpecifications action = new ImportReportSpecifications();
		Upload upload = upload("report.txt", MimeType.plain, "not a report");

		ValidationException exception = assertThrows(ValidationException.class,
				() -> action.upload(new ReportManagerExtension(), upload, new UploadException(), null));

		assertEquals("File type text/plain is not recognised as either json or zip",
				exception.getMessages().get(0).getText());
	}

	@Test
	void uploadWithMalformedJsonThrowsRecognisedReportValidationException() throws Exception {
		ImportReportSpecifications action = new ImportReportSpecifications();
		Upload upload = upload("report.json", MimeType.json, "{not-json");

		ValidationException exception = assertThrows(ValidationException.class,
				() -> action.upload(new ReportManagerExtension(), upload, new UploadException(), null));

		assertEquals("The file was not recognised as a valid report", exception.getMessages().get(0).getText());
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

	private static Upload upload(String fileName, MimeType mimeType, String contents) {
		return new Upload(fileName,
				new WebFileInputStream(new ByteArrayInputStream(contents.getBytes(java.nio.charset.StandardCharsets.UTF_8))),
				mimeType);
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
}
