package modules.admin.DataMaintenance;

import java.io.File;
import java.io.IOException;
import java.util.Date;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.impl.backup.Backup;
import org.skyve.impl.util.ThreadSafeFactory;
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
		File backupDir = null;
		
		Integer yearlyBackupRetention = dm.getYearlyBackupRetention();
		int yearly = (yearlyBackupRetention != null) ? yearlyBackupRetention.intValue() : 0;
		Integer monthlyBackupRetention = dm.getMonthlyBackupRetention();
		int monthly = (monthlyBackupRetention != null) ? monthlyBackupRetention.intValue() : 0;
		Integer weeklyBackupRetention = dm.getWeeklyBackupRetention();
		int weekly = (weeklyBackupRetention != null) ? weeklyBackupRetention.intValue() : 0;
		Integer dailyBackupRetention = dm.getDailyBackupRetention();
		int daily = (dailyBackupRetention != null) ? dailyBackupRetention.intValue() : 0;
		if (daily > 0) {
			Util.LOGGER.warning("Take backup...");
			backupDir = Backup.backup();
		}
		else {
			Util.LOGGER.warning("No daily backup taken by the BackupJob as dailyBackupRetention in DataMaintenance is null or zero");
		}

		if (backupDir != null) {
			Util.LOGGER.warning("Backup made to folder " + backupDir.getAbsolutePath());
			
			// make zip archive and delete backup folder
			String backupDirName = backupDir.getName();
			File backupZip = new File(backupDir.getParentFile(), String.format("DAILY_%s.zip", backupDirName));
			Util.LOGGER.info(String.format("Archive Backup folder %s to %s",
											backupDir.getAbsolutePath(),
											backupZip.getAbsolutePath()));
			FileUtil.createZipArchive(backupDir, backupZip);
			Util.LOGGER.info("Delete Backup folder " + backupDir.getAbsolutePath());
			FileUtil.delete(backupDir);
			backupDir = backupDir.getParentFile();

			// copy daily to weekly
			if (weekly > 0) {
				File copy = new File(backupDir, 
										String.format("WEEKLY_%s.zip", 
														ThreadSafeFactory.getDateFormat("yyyyMMWW").format(now)));
				Util.LOGGER.info(String.format("Copy Backup %s to %s",
												backupZip.getAbsolutePath(),
												copy.getAbsolutePath()));
				FileUtil.copy(backupZip, copy);
			}
			else {
				Util.LOGGER.info("No weekly backup taken by the BackupJob as weeklyBackupRetention in DataMaintenance is null or zero");
			}
			// copy daily to monthly
			if (monthly > 0) {
				File copy = new File(backupDir, 
										String.format("MONTHLY_%s.zip", 
														ThreadSafeFactory.getDateFormat("yyyyMM").format(now)));
				Util.LOGGER.info(String.format("Copy Backup %s to %s",
												backupZip.getAbsolutePath(),
												copy.getAbsolutePath()));
				FileUtil.copy(backupZip, copy);
			}
			else {
				Util.LOGGER.warning("No monthly backup taken by the BackupJob as monthlyBackupRetention in DataMaintenance is null or zero");
			}
			// copy daily to yearly
			if (yearly > 0) {
				File copy = new File(backupDir, 
										String.format("YEARLY_%s.zip", 
														ThreadSafeFactory.getDateFormat("yyyy").format(now)));
				Util.LOGGER.info(String.format("Copy Backup %s to %s",
												backupZip.getAbsolutePath(),
												copy.getAbsolutePath()));
				FileUtil.copy(backupZip, copy);
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
		getLog().add(String.format("Finished Backup of customer %s at %s",
									CORE.getUser().getCustomerName(),
									new Date()));
	}
		
	private static void cull(File backupDir, String prefix, int retain) 
	throws IOException {
		File[] files = FileUtil.listFiles(backupDir, prefix + "\\d*\\.zip", SortDirection.descending);

		for (int i = retain, l = files.length; i < l; i++) {
			Util.LOGGER.info(String.format("Cull backup %s - retention is set to %d", 
											files[i].getAbsolutePath(), 
											Integer.valueOf(retain)));
			FileUtil.delete(files[i]);
		}
	}
}
