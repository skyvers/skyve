package modules.admin.DataMaintenance;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.backup.RestoreJob;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.ContentRestoreOption;
import modules.admin.domain.DataMaintenance.RestorePreProcess;
import util.AbstractH2Test;

class RestoreJobTest extends AbstractH2Test {

	private int savedMaxExtractEntries;
	private int savedMaxExtractSizeMB;

	@BeforeEach
	void saveRestoreLimits() {
		savedMaxExtractEntries = UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES;
		savedMaxExtractSizeMB = UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB;
	}

	@AfterEach
	void restoreRestoreLimits() {
		UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES = savedMaxExtractEntries;
		UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB = savedMaxExtractSizeMB;
	}

	@Test
	@SuppressWarnings("static-method")
	void restoreJobAbortsWhenEntryCountExceedsLimit() throws Exception {
		// Arrange: backup directory for the current customer
		String customerName = CORE.getCustomer().getName();
		Path backupDirPath = Path.of(Util.getBackupDirectory(), "backup_" + customerName);
		Files.createDirectories(backupDirPath);

		String zipName = "test_entrycount.zip";
		Path zipPath = backupDirPath.resolve(zipName);
		try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(zipPath))) {
			for (int i = 1; i <= 3; i++) {
				zos.putNextEntry(new ZipEntry("file" + i + ".txt"));
				zos.write(("content" + i).getBytes(StandardCharsets.UTF_8));
				zos.closeEntry();
			}
		}

		// Limit to 2 entries — the 3-entry archive must be rejected
		UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_ENTRIES = 2;

		RestoreJob job = new RestoreJob();
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSelectedBackupName(zipName);
		bean.setContentRestoreOption(ContentRestoreOption.error);
		bean.setRestorePreProcess(RestorePreProcess.noProcessing);
		job.setBean(bean);

		// Act + Assert
		IOException ex = assertThrows(IOException.class, job::execute);
		assertTrue(ex.getMessage().startsWith("Zip archive exceeds maximum entry count"),
				"Expected entry-count message, got: " + ex.getMessage());

		List<String> log = job.getLog();
		assertTrue(log.stream().anyMatch(s -> s.contains("ABORTED")),
				"Expected ABORTED in job log, got: " + log);
	}

	@Test
	@SuppressWarnings("static-method")
	void restoreJobAbortsWhenUncompressedSizeExceedsLimit() throws Exception {
		// Arrange: a single entry whose uncompressed content exceeds a 1 MB limit
		String customerName = CORE.getCustomer().getName();
		Path backupDirPath = Path.of(Util.getBackupDirectory(), "backup_" + customerName);
		Files.createDirectories(backupDirPath);

		String zipName = "test_size.zip";
		Path zipPath = backupDirPath.resolve(zipName);
		byte[] bigContent = new byte[1024 * 1024 + 1]; // 1 MB + 1 byte
		try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(zipPath))) {
			zos.putNextEntry(new ZipEntry("big.bin"));
			zos.write(bigContent);
			zos.closeEntry();
		}

		// Limit to 1 MB — the 1 MB + 1 byte entry must be rejected
		UtilImpl.BACKUP_RESTORE_MAX_EXTRACT_SIZE_MB = 1;

		RestoreJob job = new RestoreJob();
		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSelectedBackupName(zipName);
		bean.setContentRestoreOption(ContentRestoreOption.error);
		bean.setRestorePreProcess(RestorePreProcess.noProcessing);
		job.setBean(bean);

		// Act + Assert
		IOException ex = assertThrows(IOException.class, job::execute);
		assertTrue(ex.getMessage().startsWith("Zip archive exceeds maximum uncompressed size"),
				"Expected size message, got: " + ex.getMessage());

		List<String> log = job.getLog();
		assertTrue(log.stream().anyMatch(s -> s.contains("ABORTED")),
				"Expected ABORTED in job log, got: " + log);
	}
}
