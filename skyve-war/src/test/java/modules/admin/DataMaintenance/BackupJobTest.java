package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.impl.util.UtilImpl;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class BackupJobTest extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	private String savedExternalBackupClass;
	private String savedSupportEmailAddress;

	/**
	 * External backups are off unless a test turns them on, and no support email address is
	 * configured so backup problems are logged rather than mailed (other tests in the same JVM
	 * may have set one, and there is no mail service in this test environment).
	 */
	@BeforeEach
	void disableExternalBackupsAndSupportEmail() {
		savedExternalBackupClass = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = null;
		savedSupportEmailAddress = UtilImpl.SUPPORT_EMAIL_ADDRESS;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = null;
	}

	@AfterEach
	void restoreExternalBackupsAndSupportEmail() {
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = savedExternalBackupClass;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = savedSupportEmailAddress;
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
	void executeWithLocalRetentionMovesCopiesAndCullsBackups() throws Exception {
		Path sourceZip = createFile("backup.zip");
		createFile("DAILY_20200102000000.zip");
		Path oldDaily = createFile("DAILY_20200101000000.zip");
		Path oldWeekly = createFile("WEEKLY_20200101000000.zip");
		Path oldMonthly = createFile("MONTHLY_20200101000000.zip");
		Path oldYearly = createFile("YEARLY_2020.zip");
		createFile("YEARLY_20200101000000_PROBLEMS.zip");
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(1));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		dm.setMonthlyBackupRetention(Integer.valueOf(1));
		dm.setYearlyBackupRetention(Integer.valueOf(1));
		BackupJob job = new LocalBackupJob(dm, sourceZip.toFile());

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertFalse(Files.exists(sourceZip));
		assertTrue(Files.exists(tempDir.resolve("DAILY_backup.zip")));
		assertFalse(Files.exists(oldDaily));
		assertTrue(Files.exists(tempDir.resolve(periodName("WEEKLY_", "yyyyMMWW"))));
		assertTrue(Files.exists(tempDir.resolve(periodName("MONTHLY_", "yyyyMM"))));
		assertTrue(Files.exists(tempDir.resolve(periodName("YEARLY_", "yyyy"))));
		assertFalse(Files.exists(oldWeekly));
		assertFalse(Files.exists(oldMonthly));
		assertFalse(Files.exists(oldYearly));
		assertFalse(hasFileEndingWith(".partial"));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Finished Backup")));
	}

	@Test
	void executeWithNoDailyRetentionSkipsBackupAndCompletes() throws Exception {
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		BackupJob job = new LocalBackupJob(dm, tempDir.resolve("unused.zip").toFile());

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("No daily backup taken")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Finished Backup")));
		assertFalse(Files.exists(tempDir.resolve("unused.zip")));
	}

	@Test
	void executeWithDailyOnlyRetentionLogsSkippedWeeklyAndMonthlyCopies() throws Exception {
		Path sourceZip = createFile("daily-only.zip");
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(1));
		BackupJob job = new LocalBackupJob(dm, sourceZip.toFile());

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertTrue(Files.exists(tempDir.resolve("DAILY_daily-only.zip")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("No weekly backup taken")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("No monthly backup taken")));
	}

	@Test
	void executeSkipsLocalPeriodicCopiesThatAlreadyExistForThePeriod() throws Exception {
		Path sourceZip = createFile("second.zip");
		Path existingWeekly = tempDir.resolve(periodName("WEEKLY_", "yyyyMMWW"));
		Files.writeString(existingWeekly, "first backup of the week");
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(2));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		dm.setMonthlyBackupRetention(Integer.valueOf(1));
		BackupJob job = new LocalBackupJob(dm, sourceZip.toFile());

		job.execute();

		assertEquals("first backup of the week", Files.readString(existingWeekly));
		assertTrue(Files.exists(tempDir.resolve(periodName("MONTHLY_", "yyyyMM"))));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.startsWith("Skipped weekly backup")));
		assertFalse(job.getLog().stream().anyMatch(entry -> entry.startsWith("Skipped monthly backup")));
	}

	@Test
	void executeCopiesProblemDailyWithProblemsSuffixAndDoesNotBlockLaterGoodCopy() throws Exception {
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(2));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		String weeklyName = periodName("WEEKLY_", "yyyyMMWW");
		String weeklyProblemsName = weeklyName.replace(".zip", "_PROBLEMS.zip");

		new LocalBackupJob(dm, createFile("bad_PROBLEMS.zip").toFile()).execute();

		assertTrue(Files.exists(tempDir.resolve(weeklyProblemsName)));
		assertFalse(Files.exists(tempDir.resolve(weeklyName)));

		BackupJob secondProblemJob = new LocalBackupJob(dm, createFile("bad2_PROBLEMS.zip").toFile());
		secondProblemJob.execute();

		assertTrue(secondProblemJob.getLog().stream().anyMatch(entry -> entry.startsWith("Skipped weekly backup")));
		assertFalse(Files.exists(tempDir.resolve(weeklyName)));

		new LocalBackupJob(dm, createFile("good.zip").toFile()).execute();

		assertTrue(Files.exists(tempDir.resolve(weeklyName)));
		assertTrue(Files.exists(tempDir.resolve(weeklyProblemsName)));
	}

	@Test
	void executeWithExternalBackupsCopiesEachPeriodOnceAndSkipsOnLaterRuns() throws Exception {
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(5));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		dm.setMonthlyBackupRetention(Integer.valueOf(1));
		dm.setYearlyBackupRetention(Integer.valueOf(1));
		String weeklyName = periodName("WEEKLY_", "yyyyMMWW");
		String monthlyName = periodName("MONTHLY_", "yyyyMM");
		String yearlyName = periodName("YEARLY_", "yyyy");
		FakeExternalBackup.backups.add("YEARLY_2020.zip");

		new LocalBackupJob(dm, tempDir.resolve("first.zip").toFile()).execute();

		assertThat(FakeExternalBackup.movedBackups, is(List.of("first.zip->DAILY_first.zip")));
		assertThat(FakeExternalBackup.copiedBackups, is(List.of("DAILY_first.zip->" + weeklyName,
				"DAILY_first.zip->" + monthlyName,
				"DAILY_first.zip->" + yearlyName)));

		BackupJob secondJob = new LocalBackupJob(dm, tempDir.resolve("second.zip").toFile());
		secondJob.execute();

		assertThat(FakeExternalBackup.movedBackups, is(List.of("first.zip->DAILY_first.zip", "second.zip->DAILY_second.zip")));
		assertEquals(3, FakeExternalBackup.copiedBackups.size());
		assertEquals(3, secondJob.getLog().stream().filter(entry -> entry.startsWith("Skipped ")).count());
		assertThat(FakeExternalBackup.deletedBackups, is(List.of("YEARLY_2020.zip")));
		assertFalse(FakeExternalBackup.backups.contains("YEARLY_2020.zip"));
	}

	@Test
	void executeLogsExternalPeriodicCopyFailureAndRetriesOnTheNextRun() throws Exception {
		FakeExternalBackup.throwOnCopy = true;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(2));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		String weeklyName = periodName("WEEKLY_", "yyyyMMWW");
		BackupJob job = new LocalBackupJob(dm, tempDir.resolve("first.zip").toFile());

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertThat(FakeExternalBackup.movedBackups, is(List.of("first.zip->DAILY_first.zip")));
		assertTrue(FakeExternalBackup.copiedBackups.isEmpty());
		assertFalse(job.getLog().stream().anyMatch(entry -> entry.startsWith("Failed to move external backup")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.startsWith("Failed to copy external backup for")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Could not send a backup problem email")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Finished Backup")));

		FakeExternalBackup.throwOnCopy = false;
		new LocalBackupJob(dm, tempDir.resolve("second.zip").toFile()).execute();

		assertThat(FakeExternalBackup.copiedBackups, is(List.of("DAILY_second.zip->" + weeklyName)));
	}

	@Test
	void executeSkipsPeriodicCopiesWhenTheExternalMoveFails() throws Exception {
		FakeExternalBackup.throwOnMove = true;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(1));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		dm.setMonthlyBackupRetention(Integer.valueOf(1));
		BackupJob job = new LocalBackupJob(dm, tempDir.resolve("first.zip").toFile());

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.startsWith("Failed to move external backup")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.startsWith("Skipped weekly, monthly and yearly backups")));
		assertTrue(FakeExternalBackup.copiedBackups.isEmpty());
	}

	@Test
	void executeLogsExternalExistenceCheckFailureAndDoesNotCopy() throws Exception {
		FakeExternalBackup.throwOnExists = true;
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(1));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		BackupJob job = new LocalBackupJob(dm, tempDir.resolve("first.zip").toFile());

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.startsWith("Failed to check for an existing weekly backup")));
		assertFalse(job.getLog().stream().anyMatch(entry -> entry.startsWith("Failed to copy external backup")));
		assertTrue(FakeExternalBackup.copiedBackups.isEmpty());
	}

	@Test
	void executeWithExternalBackupsCopiesProblemDailyWithProblemsSuffixAndDoesNotBlockLaterGoodCopy() throws Exception {
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		modules.admin.domain.DataMaintenance dm = modules.admin.domain.DataMaintenance.newInstance();
		dm.setDailyBackupRetention(Integer.valueOf(5));
		dm.setWeeklyBackupRetention(Integer.valueOf(1));
		String weeklyName = periodName("WEEKLY_", "yyyyMMWW");
		String weeklyProblemsName = weeklyName.replace(".zip", "_PROBLEMS.zip");

		new LocalBackupJob(dm, tempDir.resolve("bad_PROBLEMS.zip").toFile()).execute();
		BackupJob secondProblemJob = new LocalBackupJob(dm, tempDir.resolve("bad2_PROBLEMS.zip").toFile());
		secondProblemJob.execute();
		new LocalBackupJob(dm, tempDir.resolve("good.zip").toFile()).execute();

		assertThat(FakeExternalBackup.copiedBackups, is(List.of("DAILY_bad_PROBLEMS.zip->" + weeklyProblemsName,
				"DAILY_good.zip->" + weeklyName)));
		assertThat(secondProblemJob.getLog().stream().filter(entry -> entry.startsWith("Skipped weekly backup")).toList(),
				is(List.of("Skipped weekly backup as " + weeklyProblemsName + " already exists for this period")));
	}

	@Test
	void defaultFactoriesCreateBackupCollaborators() {
		ExposedBackupJob job = new ExposedBackupJob();

		assertNotNull(job.callCreateDataMaintenance());
		assertNotNull(job.callCreateBackupJob());
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
		// added oldest first; the fake lists newest first as the interface requires
		FakeExternalBackup.backups.add("DAILY_20240101000000.zip");
		FakeExternalBackup.backups.add("MONTHLY_20240101000000.zip");
		FakeExternalBackup.backups.add("DAILY_20240102000000.zip");
		FakeExternalBackup.backups.add("DAILY_20240103000000.zip");
		UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = FakeExternalBackup.class.getName();
		BackupJob job = new BackupJob();

		invokeCull(job, tempDir.toFile(), "DAILY_", 1);

		assertThat(FakeExternalBackup.deletedBackups, is(List.of("DAILY_20240102000000.zip", "DAILY_20240101000000.zip")));
		assertThat(FakeExternalBackup.backups, is(List.of("MONTHLY_20240101000000.zip", "DAILY_20240103000000.zip")));
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
		FakeExternalBackup.backups.add("DAILY_20240102000000.zip");
		FakeExternalBackup.backups.add("DAILY_20240103000000.zip");
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

	private boolean hasFileEndingWith(String suffix) throws Exception {
		try (var files = Files.list(tempDir)) {
			return files.anyMatch(path -> path.getFileName().toString().endsWith(suffix));
		}
	}

	private static String periodName(String prefix, String datePattern) {
		return prefix + CORE.getDateFormat(datePattern).format(new DateOnly()) + ".zip";
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

	private static class LocalBackupJob extends BackupJob {
		private final modules.admin.domain.DataMaintenance dm;
		private final File sourceZip;

		private LocalBackupJob(modules.admin.domain.DataMaintenance dm, File sourceZip) {
			this.dm = dm;
			this.sourceZip = sourceZip;
		}

		@Override
		protected modules.admin.domain.DataMaintenance createDataMaintenance() {
			return dm;
		}

		@Override
		protected org.skyve.impl.backup.BackupJob createBackupJob() {
			return new org.skyve.impl.backup.BackupJob() {
				@Override
				public void execute() {
					// The file is created by the test fixture; this job only reports it.
				}

				@Override
				public File getBackupZip() {
					return sourceZip;
				}
			};
		}
	}

	private static class ExposedBackupJob extends BackupJob {
		private modules.admin.domain.DataMaintenance callCreateDataMaintenance() {
			return createDataMaintenance();
		}

		private org.skyve.impl.backup.BackupJob callCreateBackupJob() {
			return createBackupJob();
		}
	}

	public static class FakeExternalBackup implements ExternalBackup {
		private static final List<String> backups = new ArrayList<>();
		private static final List<String> deletedBackups = new ArrayList<>();
		private static final List<String> copiedBackups = new ArrayList<>();
		private static final List<String> movedBackups = new ArrayList<>();
		private static boolean throwOnList;
		private static boolean throwOnDelete;
		private static boolean throwOnCopy;
		private static boolean throwOnMove;
		private static boolean throwOnExists;

		private static void reset() {
			backups.clear();
			deletedBackups.clear();
			copiedBackups.clear();
			movedBackups.clear();
			throwOnList = false;
			throwOnDelete = false;
			throwOnCopy = false;
			throwOnMove = false;
			throwOnExists = false;
		}

		/**
		 * Backups are recorded in the order they were added, so the newest is last;
		 * the interface contract is newest first, as the Azure implementation lists them.
		 */
		@Override
		public List<String> listBackups() {
			if (throwOnList) {
				throw new IllegalStateException("list failed");
			}
			List<String> result = new ArrayList<>(backups);
			java.util.Collections.reverse(result);
			return result;
		}

		@Override
		public boolean exists(String backupName) {
			if (throwOnExists) {
				throw new IllegalStateException("exists failed");
			}
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
			backups.remove(backupName);
		}

		@Override
		public void copyBackup(String srcBackupName, String destBackupName) {
			if (throwOnCopy) {
				throw new IllegalStateException("copy failed");
			}
			copiedBackups.add(srcBackupName + "->" + destBackupName);
			backups.add(destBackupName);
		}

		@Override
		public void moveBackup(String srcBackupName, String destBackupName) {
			if (throwOnMove) {
				throw new IllegalStateException("move failed");
			}
			movedBackups.add(srcBackupName + "->" + destBackupName);
			backups.remove(srcBackupName);
			backups.add(destBackupName);
		}

		@Override
		public long getFileSize(String fileName) {
			return 0L;
		}
	}
}
