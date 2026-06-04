package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;

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
public class BackupJobTest extends AbstractH2Test {
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

		assertThat(job.getPercentComplete(), is(0));
		assertThat(job.getLog().size(), is(1));
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

		assertThat(Files.exists(retained), is(true));
		assertThat(Files.exists(deleted), is(false));
		assertThat(Files.exists(anotherDeleted), is(false));
		assertThat(Files.exists(ignored), is(true));
		assertThat(job.getLog().size(), is(2));
		assertThat(job.getLog().get(0), containsString("retention is set to 1"));
	}

	@Test
	void cullDeletesOldProblemBackupsUsingSuffixRegex() throws Exception {
		Path retained = createFile("DAILY_20240103000000_PROBLEMS.zip");
		Path deleted = createFile("DAILY_20240102000000_PROBLEMS.zip");
		Path ignored = createFile("DAILY_20240101000000.zip");

		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", "_PROBLEMS", 1);

		assertThat(Files.exists(retained), is(true));
		assertThat(Files.exists(deleted), is(false));
		assertThat(Files.exists(ignored), is(true));
		assertThat(job.getLog().size(), is(1));
	}

	@Test
	void cullLeavesDirectoryUnchangedWhenNoFilesMatch() throws Exception {
		Path ignored = createFile("ignored.txt");
		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", 1);

		assertThat(Files.exists(ignored), is(true));
		assertThat(job.getLog().isEmpty(), is(true));
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
		assertThat(job.getLog().size(), is(2));
		assertThat(job.getLog().get(0), containsString("DAILY_20240102000000.zip"));
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

		private static void reset() {
			backups.clear();
			deletedBackups.clear();
		}

		@Override
		public List<String> listBackups() {
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
