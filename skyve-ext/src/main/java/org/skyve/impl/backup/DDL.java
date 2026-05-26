package org.skyve.impl.backup;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.skyve.impl.persistence.AbstractPersistence;

/**
 * Utility class for executing DDL (DROP/CREATE) SQL scripts against the
 * database as part of a backup restore operation.
 */
public class DDL {
	private DDL() {
		// nothing to see here
	}

	public static List<String> drop(File dropScript, boolean execute)
	throws Exception {
		List<String> drops = null;
		if (dropScript == null) {
			Path path = Files.createTempFile("drop", ".sql");
			try {
				AbstractPersistence.get().generateDDL(path.toString(), null, null);
				drops = BackupUtil.readScript(path.toFile());
			}
			finally {
				Files.deleteIfExists(path);
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
			Path path = Files.createTempFile("create", ".sql");
			try {
				AbstractPersistence.get().generateDDL(null, path.toString(), null);
				creates = BackupUtil.readScript(path.toFile());
			}
			finally {
				Files.deleteIfExists(path);
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
		Path path = Files.createTempFile("update", ".sql");
		try {
			AbstractPersistence.get().generateDDL(null, null, path.toString());
			updates = BackupUtil.readScript(path.toFile());
		}
		finally {
			Files.deleteIfExists(path);
		}
		if (execute) {
			BackupUtil.executeScript(updates);
		}
		return updates;
	}
}
