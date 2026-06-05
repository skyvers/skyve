package modules.admin.ReportManager;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Objects;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ReportManagerExtensionH2Test extends AbstractH2Test {
	@Test
	void marshallReportBeanWritesJsonFileInExportFolder() throws Exception {
		File folder = Files.createTempDirectory("report-manager-marshall").toFile();
		ReportManagerExtension manager = new ReportManagerExtension();
		manager.setPathToZip(folder.getAbsolutePath());
		ReportTemplateExtension report = ReportTemplate.newInstance();
		report.setName("Quarterly Report");
		report.setTemplate("template body");

		manager.marshallReportBean(report, "Quarterly Report");

		File[] files = Objects.requireNonNull(folder.listFiles());
		assertEquals(1, files.length);
		assertThat(files[0].getName(), endsWith(".json"));
		assertThat(Files.readString(files[0].toPath(), StandardCharsets.UTF_8), containsString("Quarterly Report"));
	}

	@Test
	void marshallReportBeanCleansUpAndThrowsValidationExceptionWhenFileCannotBeWritten() throws Exception {
		File pathThatIsAFile = Files.createTempFile("report-manager", ".tmp").toFile();
		ReportManagerExtension manager = new ReportManagerExtension();
		RecordingReportManagerService service = new RecordingReportManagerService();
		injectService(manager, service);
		manager.setPathToZip(pathThatIsAFile.getAbsolutePath());
		ReportTemplateExtension report = ReportTemplate.newInstance();
		report.setName("Broken Report");

		ValidationException exception = assertThrows(ValidationException.class,
				() -> manager.marshallReportBean(report, "Broken Report"));

		assertTrue(service.cleanedUp);
		assertThat(exception.getMessages().get(0).getText(), containsString("Broken Report"));
	}

	private static void injectService(ReportManagerExtension manager, ReportManagerService service) {
		try {
			Field field = ReportManagerExtension.class.getDeclaredField("reportManagerService");
			field.setAccessible(true);
			field.set(manager, service);
		} catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Failed to inject ReportManagerService", e);
		}
	}

	private static final class RecordingReportManagerService extends ReportManagerService {
		private boolean cleanedUp;

		@Override
		public void cleanUpTemporaryFiles() {
			cleanedUp = true;
		}
	}
}
