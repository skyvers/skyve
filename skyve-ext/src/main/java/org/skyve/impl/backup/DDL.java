package org.skyve.impl.backup;

import java.io.File;
import java.util.List;

import org.skyve.impl.persistence.AbstractPersistence;

public class DDL {
	private DDL() {
		// nothing to see here
	}

	public static List<String> drop(File dropScript, boolean execute)
	throws Exception {
		List<String> drops = null;
		if (dropScript == null) {
			File file = File.createTempFile("drop", ".sql");
			try {
				AbstractPersistence.get().generateDDL(file.getAbsolutePath(), null, null);
				drops = BackupUtil.readScript(file);
			}
			finally {
				if (file.exists()) {
					file.delete();
				}
			}
		}
		else {
			drops = BackupUtil.readScript(dropScript);
		}
		if (execute) {
			BackupUtil.executeScript(drops);
		}
		
		return drops;
	}
	
	public static List<String> create(File createScript, boolean execute)
	throws Exception {
		List<String> creates = null;
		if (createScript == null) {
			File file = File.createTempFile("create", ".sql");
			try {
				AbstractPersistence.get().generateDDL(null, file.getAbsolutePath(), null);
				creates = BackupUtil.readScript(file);
			}
			finally {
				if (file.exists()) {
					file.delete();
				}
			}
		}
		else {
			creates = BackupUtil.readScript(createScript);
		}
		if (execute) {
			BackupUtil.executeScript(creates);
		}
		
		return creates;
	}

	public static List<String> sync(boolean execute)
	throws Exception {
		List<String> updates = null;
		File file = File.createTempFile("update", ".sql");
		try {
			AbstractPersistence.get().generateDDL(null, null, file.getAbsolutePath());
			updates = BackupUtil.readScript(file);
		}
		finally {
			if (file.exists()) {
				file.delete();
			}
		}
		if (execute) {
			BackupUtil.executeScript(updates);
		}
		return updates;
	}
}
