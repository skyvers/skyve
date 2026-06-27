package modules.admin.ReportManager;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.file.Files;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ReportManagerServiceH2Test extends AbstractH2Test {
	@Test
	void getBasePathIncludesReportBatchPrefixAndCustomerName() {
		ReportManagerService service = new ReportManagerService();

		String basePath = service.getBasePath();

		assertThat(basePath, containsString(ReportManagerUtil.REPORTS_BATCH_PREFIX));
		assertThat(basePath, endsWith(CORE.getUser().getCustomerName()));
	}

	@Test
	void getTemporaryPreparationFolderCreatesUniqueDirectoryUnderBasePath() {
		ReportManagerService service = new ReportManagerService();

		File first = service.getTemporaryPreparationFolder();
		File second = service.getTemporaryPreparationFolder();
		try {
			assertTrue(first.exists());
			assertTrue(first.isDirectory());
			assertTrue(second.exists());
			assertTrue(second.isDirectory());
			assertNotEquals(first, second);
			assertThat(first.getParentFile().getAbsoluteFile(), is(new File(service.getBasePath()).getAbsoluteFile()));
		}
		finally {
			service.cleanUpTemporaryFiles();
		}
	}

	@Test
	void getZipFileReturnsReportManagerZipInsideBasePath() {
		ReportManagerService service = new ReportManagerService();

		File zipFile = service.getZipFile();

		assertThat(zipFile, is(notNullValue()));
		assertThat(zipFile.getParentFile().getAbsoluteFile(), is(new File(service.getBasePath()).getAbsoluteFile()));
		assertThat(zipFile.getName(), containsString(ReportManagerUtil.REPORTS_BATCH_PREFIX));
		assertThat(zipFile.getName(), endsWith(".zip"));
	}

	@Test
	void cleanUpTemporaryFilesDeletesBasePathTree() throws Exception {
		ReportManagerService service = new ReportManagerService();
		File temp = service.getTemporaryPreparationFolder();
		Files.writeString(temp.toPath().resolve("report.json"), "{}");

		service.cleanUpTemporaryFiles();

		assertFalse(new File(service.getBasePath()).exists());
	}
}
