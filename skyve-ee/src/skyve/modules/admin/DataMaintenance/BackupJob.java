package modules.admin.DataMaintenance;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Collection;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.job.Job;
import org.skyve.metadata.SortDirection;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;

import modules.admin.domain.DataMaintenance;

public class BackupJob extends Job {
	private static final long serialVersionUID = 5924130498320912107L;

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

			// copy daily to weekly
			if (weekly > 0) {
				File copy = new File(backupDir,
										String.format("WEEKLY_%s.zip",
														CORE.getDateFormat("yyyyMMWW").format(now)));
				trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
				log.add(trace);
				Util.LOGGER.info(trace);
				FileUtil.copy(dailyZip, copy);
			}
			else {
				trace = "No weekly backup taken by the BackupJob as weeklyBackupRetention in DataMaintenance is null or zero";
				log.add(trace);
				Util.LOGGER.warning(trace);
			}
			// copy daily to monthly
			if (monthly > 0) {
				File copy = new File(backupDir,
										String.format("MONTHLY_%s.zip",
														CORE.getDateFormat("yyyyMM").format(now)));
				trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
				log.add(trace);
				Util.LOGGER.info(trace);
				FileUtil.copy(dailyZip, copy);
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
				trace = String.format("Copy Backup %s to %s", backupZip.getAbsolutePath(), copy.getAbsolutePath());
				log.add(trace);
				Util.LOGGER.info(trace);
				FileUtil.copy(dailyZip, copy);
			}

			// cull daily
			cull(backupDir, "DAILY_", daily);
			// cull weekly
			cull(backupDir, "WEEKLY_", weekly);
			// cull monthly
			cull(backupDir, "MONTHLY_", monthly);
			// cull yearly
			cull(backupDir, "YEARLY_", yearly);
		}

		// TODO instance of communication instance in code - default settings

		setPercentComplete(100);
		trace = String.format("Finished Backup of customer %s at %s", CORE.getUser().getCustomerName(), new Date());
		log.add(trace);
		Util.LOGGER.info(trace);
	}

	private void cull(File backupDir, String prefix, int retain)
	throws IOException {
		Collection<String> log = getLog();
		File[] files = FileUtil.listFiles(backupDir, prefix + "\\d*\\.zip", SortDirection.descending);

		for (int i = retain, l = files.length; i < l; i++) {
			String trace = String.format("Cull backup %s - retention is set to %d",
											files[i].getAbsolutePath(),
											Integer.valueOf(retain));
			log.add(trace);
			Util.LOGGER.info(trace);
			FileUtil.delete(files[i]);
		}
	}
}
