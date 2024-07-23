package modules.admin.DataMaintenance;

import java.io.File;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.impl.backup.RestoreOptions;
import org.skyve.metadata.SortDirection;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;

import modules.admin.domain.DataMaintenance;

/**
 * Implement the RestoreOptions interface.
 * @author mike
 */
public class DataMaintenanceExtension extends DataMaintenance implements RestoreOptions {
	private static final long serialVersionUID = -838440738587384988L;

	@Override
	public PreProcess getPreProcess() {
		return PreProcess.valueOf(getRestorePreProcess().toCode());
	}

	@Override
	public ContentOption getContentOption() {
		return ContentOption.valueOf(getContentRestoreOption().toCode());
	}

	@Override
	public IndexingOption getIndexingOption() {
		return IndexingOption.valueOf(getRestoreIndexingOption().toCode());
	}
	
	public static String backupDirectoryPrefix() {
		String customerName = CORE.getUser().getCustomerName();
		String backupDirPrefix = Util.getBackupDirectory() + "backup_" + customerName;
		return backupDirPrefix;
	}
	
	public static Set<String> backups(){
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
