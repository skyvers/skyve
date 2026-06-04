package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.impl.util.UtilImpl;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class BackupJobTest extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	private String savedExternalBackupClass;

	@BeforeEach
	void disableExternalBackups() {
		savedExternalBackupClass = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = null;
	}

	@AfterEach
	void restoreExternalBackups() {
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = savedExternalBackupClass;
		FakeExternalBackup.reset();
	}

	@Test
	void cancelReturnsNull() {
		BackupJob job = new BackupJob();
		assertNull(job.cancel());
	}

	@Test
	void executeWithNoRetentionPeriodsLogsWarningAndDoesNotRunBackup() throws Exception {
		BackupJob job = new BackupJob();

		job.execute();

		assertEquals(0, job.getPercentComplete());
		assertEquals(1, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("no retention periods were set"));
	}

	@Test
	void cullDeletesOldMatchingBackupsBeyondRetention() throws Exception {
		Path retained = createFile("DAILY_20240103000000.zip");
		Path deleted = createFile("DAILY_20240102000000.zip");
		Path anotherDeleted = createFile("DAILY_20240101000000.zip");
		Path ignored = createFile("MONTHLY_20240101000000.zip");

		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", 1);

		assertTrue(Files.exists(retained));
		assertFalse(Files.exists(deleted));
		assertFalse(Files.exists(anotherDeleted));
		assertTrue(Files.exists(ignored));
		assertEquals(2, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("retention is set to 1"));
	}

	@Test
	void cullDeletesOldProblemBackupsUsingSuffixRegex() throws Exception {
		Path retained = createFile("DAILY_20240103000000_PROBLEMS.zip");
		Path deleted = createFile("DAILY_20240102000000_PROBLEMS.zip");
		Path ignored = createFile("DAILY_20240101000000.zip");

		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", "_PROBLEMS", 1);

		assertTrue(Files.exists(retained));
		assertFalse(Files.exists(deleted));
		assertTrue(Files.exists(ignored));
		assertEquals(1, job.getLog().size());
	}

	@Test
	void cullLeavesDirectoryUnchangedWhenNoFilesMatch() throws Exception {
		Path ignored = createFile("ignored.txt");
		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", 1);

		assertTrue(Files.exists(ignored));
		assertTrue(job.getLog().isEmpty());
	}

	@Test
	void cullWithNullSuffixDeletesOldMatchingBackups() throws Exception {
		Path retained = createFile("WEEKLY_20240103000000.zip");
		Path deleted = createFile("WEEKLY_20240102000000.zip");
		Path ignored = createFile("WEEKLY_20240101000000_PROBLEMS.zip");
		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "WEEKLY_", null, 1);

		assertTrue(Files.exists(retained));
		assertFalse(Files.exists(deleted));
		assertTrue(Files.exists(ignored));
		assertEquals(1, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("retention is set to 1"));
	}

	@Test
	void cullDeletesExternalBackupsBeyondRetention() throws Exception {
		FakeExternalBackup.backups.add("DAILY_20240103000000.zip");
		FakeExternalBackup.backups.add("DAILY_20240102000000.zip");
		FakeExternalBackup.backups.add("MONTHLY_20240101000000.zip");
		FakeExternalBackup.backups.add("DAILY_20240101000000.zip");
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", 1);

		assertThat(FakeExternalBackup.deletedBackups, is(List.of("DAILY_20240102000000.zip", "DAILY_20240101000000.zip")));
		assertEquals(2, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("DAILY_20240102000000.zip"));
	}

	@Test
	void cullIgnoresExternalBackupListingFailure() throws Exception {
		FakeExternalBackup.throwOnList = true;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", 1);

		assertTrue(job.getLog().isEmpty());
		assertTrue(FakeExternalBackup.deletedBackups.isEmpty());
	}

	@Test
	void cullLogsExternalBackupDeleteFailure() throws Exception {
		FakeExternalBackup.backups.add("DAILY_20240103000000.zip");
		FakeExternalBackup.backups.add("DAILY_20240102000000.zip");
		FakeExternalBackup.throwOnDelete = true;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", 1);

		assertTrue(FakeExternalBackup.deletedBackups.isEmpty());
		assertThat(job.getLog().get(0), containsString("Cull backup DAILY_20240102000000.zip"));
		assertThat(job.getLog().get(1), containsString("Failed to cull external backup DAILY_20240102000000.zip"));
	}

	private Path createFile(String fileName) throws Exception {
		Path path = tempDir.resolve(fileName);
		Files.writeString(path, fileName);
		return path;
	}

	private static void invokeCull(BackupJob job, File backupDir, String prefix, int retain) throws Exception {
		Method method = BackupJob.class.getDeclaredMethod("cull", File.class, String.class, int.class);
		method.setAccessible(true);
		method.invoke(job, backupDir, prefix, Integer.valueOf(retain));
	}

	private static void invokeCull(BackupJob job, File backupDir, String prefix, String suffix, int retain) throws Exception {
		Method method = BackupJob.class.getDeclaredMethod("cull", File.class, String.class, String.class, int.class);
		method.setAccessible(true);
		method.invoke(job, backupDir, prefix, suffix, Integer.valueOf(retain));
	}

	public static class FakeExternalBackup implements ExternalBackup {
		private static final List<String> backups = new ArrayList<>();
		private static final List<String> deletedBackups = new ArrayList<>();
		private static boolean throwOnList;
		private static boolean throwOnDelete;

		private static void reset() {
			backups.clear();
			deletedBackups.clear();
			throwOnList = false;
			throwOnDelete = false;
		}

		@Override
		public List<String> listBackups() {
			if (throwOnList) {
				throw new IllegalStateException("list failed");
			}
			return backups;
		}

		@Override
		public boolean exists(String backupName) {
			return backups.contains(backupName);
		}

		@Override
		public void downloadBackup(String backupName, OutputStream outputStream) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void uploadBackup(String backupFilePath) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void deleteBackup(String backupName) {
			if (throwOnDelete) {
				throw new IllegalStateException("delete failed");
			}
			deletedBackups.add(backupName);
		}

		@Override
		public void copyBackup(String srcBackupName, String destBackupName) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void moveBackup(String srcBackupName, String destBackupName) {
			throw new UnsupportedOperationException();
		}

		@Override
		public long getFileSize(String fileName) {
			return 0L;
		}
	}
}
