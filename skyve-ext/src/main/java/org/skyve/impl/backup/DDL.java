package org.skyve.impl.backup;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.persistence.AbstractPersistence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Utility class for executing DDL (DROP/CREATE) SQL scripts against the
 * database as part of a backup restore operation.
 */
public class DDL {
	private DDL() {
		// nothing to see here
	}

	public static @Nonnull List<String> drop(@Nullable File dropScript, boolean execute)
	throws Exception {
		List<String> drops = null;
		if (dropScript == null) {
			drops = new ArrayList<>();
			AbstractPersistence.get().generateDDL(drops, null, null);
		}
		else {
			drops = BackupUtil.readScript(dropScript);
		}
		if (execute) {
			BackupUtil.executeScript(drops);
		}
		
		return drops;
	}
	
	public static @Nonnull List<String> create(@Nullable File createScript, boolean execute)
	throws Exception {
		List<String> creates = null;
		if (createScript == null) {
			creates = new ArrayList<>();
			AbstractPersistence.get().generateDDL(null, creates, null);
		}
		else {
			creates = BackupUtil.readScript(createScript);
		}
		if (execute) {
			BackupUtil.executeScript(creates);
		}
		
		return creates;
	}

	public static @Nonnull List<String> sync(boolean execute)
	throws Exception {
		List<String> updates = null;
		updates = new ArrayList<>();
		AbstractPersistence.get().generateDDL(null, null, updates);
		if (execute) {
			BackupUtil.executeScript(updates);
		}
		return updates;
	}
}
