package modules.admin.DataMaintenance;

import java.io.File;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.metadata.SortDirection;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;

import jakarta.enterprise.inject.Default;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient DataMaintenanceService dataMaintenanceService;
 */
@Default
public class DataMaintenanceService {

	/**
	 * Gets the backup directory prefix for the current customer.
	 * 
	 * @return The backup directory prefix string
	 */
	@SuppressWarnings("static-method")
	public String backupDirectoryPrefix() {
		String customerName = CORE.getUser().getCustomerName();
		String backupDirPrefix = Util.getBackupDirectory() + "backup_" + customerName;
		return backupDirPrefix;
	}

	/**
	 * Gets a set of available backup files.
	 * 
	 * @return A sorted set of backup filenames
	 */
	public Set<String> backups() {
		File[] files = FileUtil.listFiles(new File(backupDirectoryPrefix()), ".*.zip", SortDirection.descending);
		Set<String> backups = new TreeSet<>();
		if (files != null) {
			for (File file : files) {
				backups.add(file.getName());
			}
		}
		return backups;
	}
}
