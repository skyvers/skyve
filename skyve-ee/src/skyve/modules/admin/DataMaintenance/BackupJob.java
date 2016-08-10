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
			backupDir = Backup.backup();
		}

		if (backupDir != null) {
			// make zip archive and delete backup folder
			String backupDirName = backupDir.getName();
			File backupZip = new File(backupDir.getParentFile(), String.format("DAILY_%s.zip", backupDirName));
			FileUtil.createZipArchive(backupDir, backupZip);
			FileUtil.delete(backupDir);
			backupDir = backupDir.getParentFile();

			// copy daily to weekly
			if (weekly > 0) {
				FileUtil.copy(backupZip, 
								new File(backupDir, 
											String.format("WEEKLY_%s.zip", 
															ThreadSafeFactory.getDateFormat("yyyyMMWW").format(now))));
			}
			// copy daily to monthly
			if (monthly > 0) {
				FileUtil.copy(backupZip, 
								new File(backupDir, 
											String.format("MONTHLY_%s.zip", 
															ThreadSafeFactory.getDateFormat("yyyyMM").format(now))));
			}
			// copy daily to yearly
			if (yearly > 0) {
				FileUtil.copy(backupZip, 
								new File(backupDir, 
											String.format("YEARLY_%s.zip", 
															ThreadSafeFactory.getDateFormat("yyyy").format(now))));
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
		getLog().add("Finished Backup of customer " + CORE.getUser().getCustomerName() + " at " + new Date());
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
