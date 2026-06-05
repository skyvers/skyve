package modules.admin.DataMaintenance;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.File;
import java.nio.file.Path;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;

import util.AbstractH2Test;

class DataMaintenanceServiceH2Test extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	private final String savedBackupDirectory = UtilImpl.BACKUP_DIRECTORY;

	@AfterEach
	void restoreBackupDirectory() {
		UtilImpl.BACKUP_DIRECTORY = savedBackupDirectory;
	}

	@Test
	void backupDirectoryPrefixUsesConfiguredBackupDirectoryAndCurrentCustomer() {
		UtilImpl.BACKUP_DIRECTORY = tempDir.toString() + File.separator;

		String prefix = new DataMaintenanceService().backupDirectoryPrefix();

		assertThat(prefix, is(tempDir + File.separator + "backup_" + CORE.getUser().getCustomerName()));
	}
}
