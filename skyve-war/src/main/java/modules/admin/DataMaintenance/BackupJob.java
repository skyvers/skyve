package modules.admin.DataMaintenance;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.Job;
import org.skyve.metadata.SortDirection;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;

import modules.admin.domain.DataMaintenance;

public class BackupJob extends Job {
	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {
		DateOnly now = new DateOnly();
		DataMaintenance dm = DataMaintenance.newInstance();
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
		} else {
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
			if (ExternalBackup.areExternalBackupsEnabled()) {
				try {
					ExternalBackup.getInstance().moveBackup(backupZip.getName(), dailyZip.getName());
				} catch (Exception e) {
					trace = String.format("Failed to move external backup for %s from %s to %s",
							UtilImpl.ARCHIVE_NAME, backupZip.getName(), dailyZip.getName());
					log.add(trace);
					Util.LOGGER.warning(trace);
					e.printStackTrace();
					org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
				}
			} else {
				if (Files.move(backupZip.toPath(), dailyZip.toPath(), StandardCopyOption.REPLACE_EXISTING) == null) {
					throw new IOException("Could not rename " + backupZip.getPath() + " to " + dailyZip.getPath());
				}
				trace = String.format("Backup moved from %s to %s", backupZip.getAbsolutePath(), dailyZip.getAbsolutePath());
				log.add(trace);
				Util.LOGGER.info(trace);
			}

			// copy daily to weekly
			if (weekly > 0) {
				File copy = new File(backupDir,
						String.format("WEEKLY_%s.zip",
								CORE.getDateFormat("yyyyMMWW").format(now)));
				if (ExternalBackup.areExternalBackupsEnabled()) {
					try {
						ExternalBackup.getInstance().copyBackup(dailyZip.getName(), copy.getName());
					} catch (Exception e) {
						trace = String.format("Failed to copy external backup for %s from %s to %s",
								UtilImpl.ARCHIVE_NAME, dailyZip.getName(), copy.getName());
						log.add(trace);
						Util.LOGGER.warning(trace);
						e.printStackTrace();
						org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
					}
				} else {
					trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
					log.add(trace);
					Util.LOGGER.info(trace);
					FileUtil.copy(dailyZip, copy);
				}
			} else {
				trace = "No weekly backup taken by the BackupJob as weeklyBackupRetention in DataMaintenance is null or zero";
				log.add(trace);
				Util.LOGGER.warning(trace);
			}
			// copy daily to monthly
			if (monthly > 0) {
				File copy = new File(backupDir,
						String.format("MONTHLY_%s.zip",
								CORE.getDateFormat("yyyyMM").format(now)));
				if (ExternalBackup.areExternalBackupsEnabled()) {
					try {
						ExternalBackup.getInstance().copyBackup(dailyZip.getName(), copy.getName());
					} catch (Exception e) {
						trace = String.format("Failed to copy external backup for %s from %s to %s",
								UtilImpl.ARCHIVE_NAME, dailyZip.getName(), copy.getName());
						log.add(trace);
						Util.LOGGER.warning(trace);
						e.printStackTrace();
						org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
					}
				} else {
					trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
					log.add(trace);
					Util.LOGGER.info(trace);
					FileUtil.copy(dailyZip, copy);
				}
			}
			else {
				trace = "No monthly backup taken by the BackupJob as monthlyBackupRetention in DataMaintenance is null or zero";
				log.add(trace);
				Util.LOGGER.warning(trace);
			}
			// copy daily to yearly
			if (yearly > 0) {
				File copy = new File(backupDir,
						String.format("YEARLY_%s.zip",
								CORE.getDateFormat("yyyy").format(now)));
				if (ExternalBackup.areExternalBackupsEnabled()) {
					try {
						ExternalBackup.getInstance().copyBackup(dailyZip.getName(), copy.getName());
					} catch (@SuppressWarnings("unused") Exception e) {
						trace = String.format("Failed to copy external backup from %s to %s", dailyZip.getName(), copy.getName());
						log.add(trace);
						Util.LOGGER.warning(trace);
						org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
					}
				} else {
					trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
					log.add(trace);
					Util.LOGGER.info(trace);
					FileUtil.copy(dailyZip, copy);
				}
			}

			// cull daily
			cull(backupDir, "DAILY_", daily);
			cull(backupDir, "DAILY_", "_PROBLEMS", daily * 2);
			// cull weekly
			cull(backupDir, "WEEKLY_", weekly);
			cull(backupDir, "WEEKLY_", "_PROBLEMS", weekly * 2);
			// cull monthly
			cull(backupDir, "MONTHLY_", monthly);
			cull(backupDir, "MONTHLY_", "_PROBLEMS", monthly * 2);
			// cull yearly
			cull(backupDir, "YEARLY_", "_PROBLEMS", yearly * 2);
		}

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
		List<String> log = getLog();
		final String regex = prefix + "\\d*" + ((suffix == null) ? "" : suffix) + "\\.zip";
		File[] files = FileUtil.listFiles(backupDir, regex, SortDirection.descending);
		if (files != null) {
			for (int i = retain, l = files.length; i < l; i++) {
				String trace = String.format("Cull backup %s - retention is set to %d",
						files[i].getAbsolutePath(),
						Integer.valueOf(retain));
				log.add(trace);
				Util.LOGGER.info(trace);
				FileUtil.delete(files[i]);
			}
		}
		if (ExternalBackup.areExternalBackupsEnabled()) {
			try {
				final ExternalBackup externalBackup = ExternalBackup.getInstance();
				final List<String> backups = externalBackup.listBackups();
				final List<String> matchingBackups = backups.stream().filter(backup -> backup.matches(regex))
						.collect(Collectors.toList());
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
						org.skyve.impl.backup.BackupJob.emailProblem(log, trace);
					}
				}
			} catch (Exception e) {
				Util.LOGGER.warning("Failed to cull external backups " + e.getMessage());
			}
		}
	}
}
