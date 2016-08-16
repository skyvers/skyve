package org.skyve.impl.backup;

import java.io.File;
import java.util.ArrayList;
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
	
	public static List<String> create(File createScript, boolean execute)
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

	public static List<String> sync(boolean execute)
	throws Exception {
		List<String> updates = new ArrayList<>();
		AbstractPersistence.get().generateDDL(null, null, updates);
		if (execute) {
			BackupUtil.executeScript(updates);
		}
		return updates;
	}
}
