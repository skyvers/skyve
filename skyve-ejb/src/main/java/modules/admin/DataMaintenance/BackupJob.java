package modules.admin.DataMaintenance;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.job.Job;
import org.skyve.metadata.SortDirection;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;

import modules.admin.domain.DataMaintenance;

public class BackupJob extends Job {

	private final String WEEKLY_BACKUP_PREFIX = "WEEKLY";
	private final String MONTHLY_BACKUP_PREFIX = "MONTHLY";
	private final String YEARLY_BACKUP_PREFIX = "YEARLY";
	private final long MILLIS_IN_WEEK = 604800000L;
	private final long MILLIS_IN_MONTH = 2629800000L;
	private final long MILLIS_IN_YEAR = 31556952000L;

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {
		DateOnly now = new DateOnly();
		DataMaintenance dm = DataMaintenance.newInstance();
		File backupZip = null;
		Collection<String> log = getLog();
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
			Util.LOGGER.info(trace);
			setPercentComplete(0);
			return;
		}

		if (daily > 0) {
			trace = "Take backup...";
			log.add(trace);
			Util.LOGGER.warning(trace);
			org.skyve.impl.backup.BackupJob backupJob = new org.skyve.impl.backup.BackupJob();
			execute(backupJob);
			backupZip = backupJob.getBackupZip();
		}
		else {
			trace = "No daily backup taken by the BackupJob as dailyBackupRetention in DataMaintenance is null or zero";
			log.add(trace);
			Util.LOGGER.warning(trace);
		}

		if (backupZip != null) {
			trace = "Backup made to zip " + backupZip.getAbsolutePath();
			log.add(trace);
			Util.LOGGER.warning(trace);

			// move the zip archive
			File backupDir = backupZip.getParentFile();
			File dailyZip = new File(backupDir, "DAILY_" + backupZip.getName());
			if (Files.move(backupZip.toPath(), dailyZip.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
				throw new IOException("Could not rename " + backupZip.getPath() + " to " + dailyZip.getPath());
			}
			trace = String.format("Backup moved from %s to %s", backupZip.getAbsolutePath(), dailyZip.getAbsolutePath());
			log.add(trace);
			Util.LOGGER.info(trace);
			if (ExternalBackup.areExternalBackupsEnabled()) {
				try {
					ExternalBackup.getInstance().moveBackup(backupZip.getName(), dailyZip.getName());
				} catch (@SuppressWarnings("unused") Exception e) {
					trace = String.format("Failed to move external backup from %s to %s", backupZip.getName(), dailyZip.getName());
					log.add(trace);
					Util.LOGGER.warning(trace);
				}
			}

			// copy daily to weekly
			if (weekly > 0) {
				// Check existing weekly backup exists
				boolean doWeeklyBackup = false;
				// Retrieve file from the backup directory where it is the last WEEKLY backup file
				List<File> files = Files.walk(backupDir.toPath())
						.filter(Files::isRegularFile)
						.map(Path::toFile)
						.filter(f -> f.getName().startsWith(WEEKLY_BACKUP_PREFIX))
						.sorted(Comparator.comparing(File::lastModified).reversed())
						.limit(1)
						.collect(Collectors.toList());
				
				// check previous weekly backup exists and is at least a week old
				if (files.size() > 0) {
					File file = files.get(0);
					long timeDifference = System.currentTimeMillis() - file.lastModified();
					if (timeDifference >= MILLIS_IN_WEEK) {
						doWeeklyBackup = true;
					}
				}
				else {
					// First weekly backup
					doWeeklyBackup = true;
				}

				// Weekly backup should be made if it is the first weekly backup or if the most recent weekly backup is over a week
				// old
				if (doWeeklyBackup) {
					File copy = new File(backupDir,
							String.format("WEEKLY_%s.zip",
									CORE.getDateFormat("yyyyMMWW").format(now)));
					trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
					log.add(trace);
					Util.LOGGER.info(trace);
					FileUtil.copy(dailyZip, copy);
					if (ExternalBackup.areExternalBackupsEnabled()) {
						try {
							ExternalBackup.getInstance().copyBackup(dailyZip.getName(), copy.getName());
						} catch (@SuppressWarnings("unused") Exception e) {
							trace = String.format("Failed to copy external backup from %s to %s", dailyZip.getName(),
									copy.getName());
							log.add(trace);
							Util.LOGGER.warning(trace);
						}
					}
				}
			}
			else {
				trace = "No weekly backup taken by the BackupJob as weeklyBackupRetention in DataMaintenance is null or zero";
				log.add(trace);
				Util.LOGGER.warning(trace);
			}
			// copy daily to monthly
			if (monthly > 0) {
				// Check existing monthly backup is one month old
				boolean doMonthlyBackup = false;
				// Retrieve file from the backup directory where it is the last MONTHLY backup file
				List<File> files = Files.walk(backupDir.toPath())
						.filter(Files::isRegularFile)
						.map(Path::toFile)
						.filter(f -> f.getName().startsWith(MONTHLY_BACKUP_PREFIX))
						.sorted(Comparator.comparing(File::lastModified).reversed())
						.limit(1)
						.collect(Collectors.toList());
				
				// check previous Monthly backup exists and is at least a week old
				if (files.size() > 0) {
					File file = files.get(0);
					long timeDifference = System.currentTimeMillis() - file.lastModified();
					if (timeDifference >= MILLIS_IN_MONTH) {
						doMonthlyBackup = true;
					}
				}
				else {
					// First monthly backup
					doMonthlyBackup = true;
				}

				// Monthly backup should be made if it is the first monthly backup or if the most recent monthly backup is over a
				// month old
				if (doMonthlyBackup) {
					File copy = new File(backupDir,
							String.format("MONTHLY_%s.zip",
									CORE.getDateFormat("yyyyMM").format(now)));
					trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
					log.add(trace);
					Util.LOGGER.info(trace);
					FileUtil.copy(dailyZip, copy);
					if (ExternalBackup.areExternalBackupsEnabled()) {
						try {
							ExternalBackup.getInstance().copyBackup(dailyZip.getName(), copy.getName());
						} catch (@SuppressWarnings("unused") Exception e) {
							trace = String.format("Failed to copy external backup from %s to %s", dailyZip.getName(),
									copy.getName());
							log.add(trace);
							Util.LOGGER.warning(trace);
						}
					}
				}
			}
			else {
				trace = "No monthly backup taken by the BackupJob as monthlyBackupRetention in DataMaintenance is null or zero";
				log.add(trace);
				Util.LOGGER.warning(trace);
			}
			// copy daily to yearly
			if (yearly > 0) {
				// Check existing yearly backup is one month old
				boolean doYearlyBackup = false;
				// Retrieve file from the backup directory where it is the last YEARLY backup file
				List<File> files = Files.walk(backupDir.toPath())
						.filter(Files::isRegularFile)
						.map(Path::toFile)
						.filter(f -> f.getName().startsWith(YEARLY_BACKUP_PREFIX))
						.sorted(Comparator.comparing(File::lastModified).reversed())
						.limit(1)
						.collect(Collectors.toList());

				// check previous Monthly backup exists and is at least a week old
				if (files.size() > 0) {
					File file = files.get(0);
					long timeDifference = System.currentTimeMillis() - file.lastModified();
					if (timeDifference >= MILLIS_IN_YEAR) {
						doYearlyBackup = true;
					}
				} else {
					// First yearly backup
					doYearlyBackup = true;
				}

				// Yearly backup should be made if it is the first yearly backup or if the most recent yearly backup is over a year
				// old
				if (doYearlyBackup) {
					File copy = new File(backupDir,
							String.format("YEARLY_%s.zip",
									CORE.getDateFormat("yyyy").format(now)));
					trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
					log.add(trace);
					Util.LOGGER.info(trace);
					FileUtil.copy(dailyZip, copy);
					if (ExternalBackup.areExternalBackupsEnabled()) {
						try {
							ExternalBackup.getInstance().copyBackup(dailyZip.getName(), copy.getName());
						} catch (@SuppressWarnings("unused") Exception e) {
							trace = String.format("Failed to copy external backup from %s to %s", dailyZip.getName(),
									copy.getName());
							log.add(trace);
							Util.LOGGER.warning(trace);
						}
					}
				}
			}

			// cull daily
			cull(backupDir, "DAILY_", daily);
			cull(backupDir, "DAILY_", "_PROBLEMS", daily*2);
			// cull weekly
			cull(backupDir, "WEEKLY_", weekly);
			cull(backupDir, "WEEKLY_", "_PROBLEMS", weekly*2);
			// cull monthly
			cull(backupDir, "MONTHLY_", monthly);
			cull(backupDir, "MONTHLY_", "_PROBLEMS", monthly*2);
			// cull yearly
			cull(backupDir, "YEARLY_", "_PROBLEMS", yearly*2);
		}

		// TODO instance of communication instance in code - default settings

		setPercentComplete(100);
		trace = String.format("Finished Backup of customer %s at %s", CORE.getUser().getCustomerName(), new Date());
		log.add(trace);
		Util.LOGGER.info(trace);
	}

	private void cull(File backupDir, String prefix, int retain)
			throws IOException {
		cull(backupDir, prefix, "", retain);
	}

	private void cull(File backupDir, String prefix, String suffix, int retain)
	throws IOException {
		Collection<String> log = getLog();
		final String regex = prefix + "\\d*" + ((suffix == null) ? "" : suffix) + "\\.zip";
		File[] files = FileUtil.listFiles(backupDir, regex, SortDirection.descending);

		for (int i = retain, l = files.length; i < l; i++) {
			String trace = String.format("Cull backup %s - retention is set to %d",
											files[i].getAbsolutePath(),
											Integer.valueOf(retain));
			log.add(trace);
			Util.LOGGER.info(trace);
			FileUtil.delete(files[i]);
		}
		if (ExternalBackup.areExternalBackupsEnabled()) {
			try {
				final ExternalBackup externalBackup = ExternalBackup.getInstance();
				final List<String> backups = externalBackup.listBackups();
				final List<String> matchingBackups = backups.stream().filter(backup -> backup.matches(regex)).collect(Collectors.toList());
				for (int i = retain, l = matchingBackups.size(); i < l; i++) {
					String trace = String.format("Cull backup %s - retention is set to %d",
							matchingBackups.get(i),
							Integer.valueOf(retain));
					log.add(trace);
					Util.LOGGER.info(trace);
					try {
						ExternalBackup.getInstance().deleteBackup(matchingBackups.get(i));
					} catch (@SuppressWarnings("unused") Exception e) {
						trace = String.format("Failed to cull external backup %s", matchingBackups.get(i));
						log.add(trace);
						Util.LOGGER.warning(trace);
					}
				}
			} catch (Exception e) {
				Util.LOGGER.warning("Failed to cull external backups " + e.getMessage());
			}
		}
	}
}
