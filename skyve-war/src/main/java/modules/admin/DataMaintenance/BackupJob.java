package modules.admin.DataMaintenance;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Date;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.Job;
import org.skyve.metadata.SortDirection;
import org.skyve.util.FileUtil;

import modules.admin.domain.DataMaintenance;

/**
 * Runs background backup generation and records progress for Data Maintenance.
 */
public class BackupJob extends Job {
	private static final String COPY_BACKUP_FORMAT = "Copy Backup %s to %s";
	private static final String SKIPPED_FORMAT = "Skipped %s backup as %s already exists for this period";
	private static final String DAILY_PREFIX = "DAILY_";
	private static final String WEEKLY_PREFIX = "WEEKLY_";
	private static final String MONTHLY_PREFIX = "MONTHLY_";
	private static final String YEARLY_PREFIX = "YEARLY_";
	private static final String PROBLEMS_SUFFIX = "_PROBLEMS";
	private static final String ZIP_SUFFIX = ".zip";

	/**
	 * Performs the cancel operation.
	 * @return the operation result
	 */
	@Override
	public String cancel() {
		return null;
	}

	/**
	 * Performs the execute operation.
	 * @throws Exception if the operation fails
	 */
	@Override
	@SuppressWarnings({"java:S3776", "java:S6541"}) // complexity OK
	public void execute() throws Exception {
		DateOnly now = new DateOnly();
		DataMaintenance dm = createDataMaintenance();
		File backupZip = null;
		List<String> log = getLog();
		String trace;

		Integer yearlyBackupRetention = dm.getYearlyBackupRetention();
		int yearly = (yearlyBackupRetention != null) ? yearlyBackupRetention.intValue() : 0;
		Integer monthlyBackupRetention = dm.getMonthlyBackupRetention();
		int monthly = (monthlyBackupRetention != null) ? monthlyBackupRetention.intValue() : 0;
		Integer weeklyBackupRetention = dm.getWeeklyBackupRetention();
		int weekly = (weeklyBackupRetention != null) ? weeklyBackupRetention.intValue() : 0;
		Integer dailyBackupRetention = dm.getDailyBackupRetention();
		int daily = (dailyBackupRetention != null) ? dailyBackupRetention.intValue() : 0;

		if (yearly == 0 && monthly == 0 && weekly == 0 && daily == 0) {
			// warn the user if no backup was set and shortcut out
			trace = "No backup taken by the BackupJob as no retention periods were set on the Data Maintenance Backup/Restore tab.";
			log.add(trace);
			LOGGER.info(trace);
			setPercentComplete(0);
			return;
		}

		if (daily > 0) {
			trace = "Take backup...";
			log.add(trace);
			LOGGER.warn(trace);
			org.skyve.impl.backup.BackupJob backupJob = createBackupJob();
			execute(backupJob);
			backupZip = backupJob.getBackupZip();
		} else {
			trace = "No daily backup taken by the BackupJob as dailyBackupRetention in DataMaintenance is null or zero";
			log.add(trace);
			LOGGER.warn(trace);
		}

		if (backupZip != null) {
			trace = "Backup made to zip " + backupZip.getAbsolutePath();
			log.add(trace);
			LOGGER.warn(trace);

			// move the zip archive
			File backupDir = backupZip.getParentFile();
			File dailyZip = new File(backupDir, DAILY_PREFIX + backupZip.getName());
			if (ExternalBackup.areExternalBackupsEnabled()) {
				try {
					ExternalBackup.getInstance().moveBackup(backupZip.getName(), dailyZip.getName());
				} catch (Exception e) {
					trace = String.format("Failed to move external backup for %s from %s to %s",
							UtilImpl.ARCHIVE_NAME, backupZip.getName(), dailyZip.getName());
					log.add(trace);
					LOGGER.warn(trace, e);
					org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
				}
			} else {
				if (Files.move(backupZip.toPath(), dailyZip.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
					throw new IOException("Could not rename " + backupZip.getPath() + " to " + dailyZip.getPath());
				}
				trace = String.format("Backup moved from %s to %s", backupZip.getAbsolutePath(), dailyZip.getAbsolutePath());
				log.add(trace);
				LOGGER.info(trace);
			}

			// copy daily to weekly, monthly and yearly (at most once per period)
			copyPeriodic(backupDir, dailyZip, "weekly", WEEKLY_PREFIX, "yyyyMMWW", now, weekly);
			copyPeriodic(backupDir, dailyZip, "monthly", MONTHLY_PREFIX, "yyyyMM", now, monthly);
			copyPeriodic(backupDir, dailyZip, "yearly", YEARLY_PREFIX, "yyyy", now, yearly);

			// cull daily
			cull(backupDir, DAILY_PREFIX, daily);
			cull(backupDir, DAILY_PREFIX, PROBLEMS_SUFFIX, daily * 2);
			// cull weekly
			cull(backupDir, WEEKLY_PREFIX, weekly);
			cull(backupDir, WEEKLY_PREFIX, PROBLEMS_SUFFIX, weekly * 2);
			// cull monthly
			cull(backupDir, MONTHLY_PREFIX, monthly);
			cull(backupDir, MONTHLY_PREFIX, PROBLEMS_SUFFIX, monthly * 2);
			// cull yearly
			cull(backupDir, YEARLY_PREFIX, yearly);
			cull(backupDir, YEARLY_PREFIX, PROBLEMS_SUFFIX, yearly * 2);
		}

		setPercentComplete(100);
		trace = String.format("Finished Backup of customer %s at %s", CORE.getUser().getCustomerName(), new Date());
		log.add(trace);
		LOGGER.info(trace);
	}

	@SuppressWarnings("static-method") // test seam
	protected DataMaintenance createDataMaintenance() {
		return DataMaintenance.newInstance();
	}

	@SuppressWarnings("static-method") // test seam
	protected org.skyve.impl.backup.BackupJob createBackupJob() {
		return new org.skyve.impl.backup.BackupJob();
	}

	/**
	 * Copy the daily backup to a weekly, monthly or yearly backup named for the current period.
	 * <p>
	 * The copy is made at most once per period: if a backup for the period already exists
	 * (locally or externally) the copy is skipped, so external storage sees one new
	 * weekly/monthly/yearly upload per period rather than one per day.
	 * A daily backup with problems is copied with the problems suffix and does not
	 * stand in for a good backup, so a later good daily still produces the period's backup.
	 *
	 * @param backupDir the local backup directory
	 * @param dailyZip the daily backup to copy (may not exist locally when external backups are enabled)
	 * @param period the period name used in log messages (weekly, monthly or yearly)
	 * @param prefix the backup name prefix for the period
	 * @param datePattern the date format used to name the period's backup
	 * @param now the date of this backup run
	 * @param retention the retention count for the period (zero or less disables the copy)
	 * @throws Exception if a local copy fails
	 */
	private void copyPeriodic(File backupDir,
								File dailyZip,
								String period,
								String prefix,
								String datePattern,
								DateOnly now,
								int retention)
	throws Exception {
		List<String> log = getLog();
		String trace;

		if (retention <= 0) {
			trace = String.format("No %s backup taken by the BackupJob as %sBackupRetention in DataMaintenance is null or zero", period, period);
			log.add(trace);
			LOGGER.warn(trace);
			return;
		}

		boolean problem = dailyZip.getName().endsWith(PROBLEMS_SUFFIX + ZIP_SUFFIX);
		String periodName = prefix + CORE.getDateFormat(datePattern).format(now);
		String goodName = periodName + ZIP_SUFFIX;
		String copyName = problem ? periodName + PROBLEMS_SUFFIX + ZIP_SUFFIX : goodName;
		File copy = new File(backupDir, copyName);

		if (ExternalBackup.areExternalBackupsEnabled()) {
			try {
				ExternalBackup externalBackup = ExternalBackup.getInstance();
				String existing = externalBackup.exists(goodName) ? goodName : null;
				if ((existing == null) && problem && externalBackup.exists(copyName)) {
					existing = copyName;
				}
				if (existing != null) {
					trace = String.format(SKIPPED_FORMAT, period, existing);
					log.add(trace);
					LOGGER.info(trace);
					return;
				}
				externalBackup.copyBackup(dailyZip.getName(), copyName);
			} catch (Exception e) {
				trace = String.format("Failed to copy external backup for %s from %s to %s",
						UtilImpl.ARCHIVE_NAME, dailyZip.getName(), copyName);
				log.add(trace);
				LOGGER.warn(trace, e);
				org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
			}
		} else {
			File good = new File(backupDir, goodName);
			File existing = good.exists() ? good : ((problem && copy.exists()) ? copy : null);
			if (existing != null) {
				trace = String.format(SKIPPED_FORMAT, period, existing.getAbsolutePath());
				log.add(trace);
				LOGGER.info(trace);
				return;
			}
			trace = String.format(COPY_BACKUP_FORMAT, dailyZip.getAbsolutePath(), copy.getAbsolutePath());
			log.add(trace);
			LOGGER.info(trace);
			FileUtil.copy(dailyZip, copy);
		}
	}

	private void cull(File backupDir, String prefix, int retain)
			throws IOException {
		cull(backupDir, prefix, "", retain);
	}

	private void cull(File backupDir, String prefix, String suffix, int retain)
			throws IOException {
		List<String> log = getLog();
		final String regex = prefix + "\\d*" + ((suffix == null) ? "" : suffix) + "\\.zip";
		File[] files = FileUtil.listFiles(backupDir, regex, SortDirection.descending);
		if (files != null) {
			for (int i = retain, l = files.length; i < l; i++) {
				String trace = String.format("Cull backup %s - retention is set to %d",
						files[i].getAbsolutePath(),
						Integer.valueOf(retain));
				log.add(trace);
				LOGGER.info(trace);
				FileUtil.delete(files[i]);
			}
		}
		if (ExternalBackup.areExternalBackupsEnabled()) {
			try {
				final ExternalBackup externalBackup = ExternalBackup.getInstance();
				final List<String> backups = externalBackup.listBackups();
				final List<String> matchingBackups = backups.stream().filter(backup -> backup.matches(regex))
						.toList();
				for (int i = retain, l = matchingBackups.size(); i < l; i++) {
					String trace = String.format("Cull backup %s - retention is set to %d",
							matchingBackups.get(i),
							Integer.valueOf(retain));
					log.add(trace);
					LOGGER.info(trace);
					try {
						ExternalBackup.getInstance().deleteBackup(matchingBackups.get(i));
					} catch (@SuppressWarnings("unused") Exception e) {
						trace = String.format("Failed to cull external backup %s", matchingBackups.get(i));
						log.add(trace);
						LOGGER.warn(trace);
						org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
					}
				}
			} catch (Exception e) {
				LOGGER.warn("Failed to cull external backups {}", e.getMessage());
			}
		}
	}
}
