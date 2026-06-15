package modules.admin.ReportManager.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;

import modules.admin.ReportManager.ReportManagerExtension;
import modules.admin.ReportManager.ReportManagerService;
import modules.admin.ReportManager.ReportManagerUtil;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportTemplate;
import util.AbstractH2Test;

/**
 * Tests for ExportReportSpecifications action.
 */
@SuppressWarnings("static-method")
class ExportReportSpecificationsTest extends AbstractH2Test {

	@Test
	void prepareWithEmptyCurrentReportsThrowsValidationException() {
		ExportReportSpecifications action = new ExportReportSpecifications();
		ReportManagerExtension bean = new ReportManagerExtension();
		// currentReports is initialized to an empty ChangeTrackingArrayList
		// so size() == 0 → throws ValidationException before any CDI call
		assertThrows(ValidationException.class, () -> action.prepare(bean, null));
	}

	@Test
	void prepareWithValidReportsCreatesPreparationFolderAndMarshallsEachReport() throws Exception {
		FakeReportManagerService service = new FakeReportManagerService();
		ExportReportSpecifications action = actionWithService(service);
		RecordingReportManager bean = new RecordingReportManager();
		ReportTemplateExtension first = validTemplate("First Report");
		ReportTemplateExtension second = validTemplate("Second Report");
		bean.getCurrentReports().add(first);
		bean.getCurrentReports().add(second);

		action.prepare(bean, null);

		assertThat(bean.getPathToZip(), is(service.folder.getAbsolutePath()));
		assertThat(bean.marshalledNames, is(List.of("First Report", "Second Report")));
		assertThat(bean.marshalledReports, is(List.of(first, second)));
	}

	@Test
	void downloadZipsPreparedFolderAndCleansUpTemporaryFiles() throws Exception {
		File folder = Files.createTempDirectory("report-manager-export").toFile();
		Files.writeString(folder.toPath().resolve("report.json"), "{\"name\":\"Report\"}");
		FakeReportManagerService service = new FakeReportManagerService();
		ExportReportSpecifications action = actionWithService(service);
		ReportManagerExtension bean = new ReportManagerExtension();
		bean.setPathToZip(folder.getAbsolutePath());

		Download download = action.download(bean, null);

		assertTrue(download.getFileName().startsWith(ReportManagerUtil.REPORTS_BATCH_PREFIX));
		assertTrue(download.getFileName().endsWith(".zip"));
		assertTrue(download.getBytes().length > 0);
		assertTrue(service.cleanedUp);
	}

	private static ReportTemplateExtension validTemplate(String name) {
		ReportTemplateExtension template = ReportTemplate.newInstance();
		template.setName(name);
		template.setTemplateName(name + ".html");
		return template;
	}

	private static ExportReportSpecifications actionWithService(ReportManagerService service) {
		ExportReportSpecifications action = new ExportReportSpecifications();
		try {
			Field field = ExportReportSpecifications.class.getDeclaredField("reportManagerService");
			field.setAccessible(true);
			field.set(action, service);
		} catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Failed to inject ReportManagerService", e);
		}
		return action;
	}

	private static final class RecordingReportManager extends ReportManagerExtension {
		private static final long serialVersionUID = 8340455231182687254L;

		private final List<ReportTemplate> marshalledReports = new ArrayList<>();
		private final List<String> marshalledNames = new ArrayList<>();

		@Override
		public void marshallReportBean(ReportTemplate report, String name) {
			marshalledReports.add(report);
			marshalledNames.add(name);
		}
	}

	private static final class FakeReportManagerService extends ReportManagerService {
		private final File folder;
		private boolean cleanedUp;

		private FakeReportManagerService() throws Exception {
			folder = Files.createTempDirectory("report-manager-prepare").toFile();
		}

		@Override
		public File getTemporaryPreparationFolder() {
			return folder;
		}

		@Override
		public void cleanUpTemporaryFiles() {
			cleanedUp = true;
			try {
				File[] files = folder.listFiles();
				if (files != null) {
					for (File file : files) {
						Files.delete(file.toPath());
					}
				}
				Files.delete(folder.toPath());
			}
			catch (IOException e) {
				throw new AssertionError("Failed to clean up temporary report files", e);
			}
		}
	}
}
