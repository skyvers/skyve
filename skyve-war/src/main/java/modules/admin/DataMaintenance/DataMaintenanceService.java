package modules.admin.DataMaintenance;

import org.skyve.CORE;
import org.skyve.util.Util;

import jakarta.enterprise.inject.Default;

/**
 * Provides service operations for admin data maintenance.
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
}
