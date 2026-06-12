package org.skyve.impl.backup;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertThrows;

import java.io.OutputStream;
import java.util.List;

import org.junit.Test;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
public class ExternalBackupTest {

	@Test
	public void areExternalBackupsEnabledReturnsFalseByDefault() {
		// BACKUP_EXTERNAL_BACKUP_CLASS defaults to null
		assertFalse(ExternalBackup.areExternalBackupsEnabled());
	}

	@Test
	public void getInstanceThrowsWhenConfigurationCannotCreateBackup() {
		String saved = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		try {
			String[] invalidClassNames = {null, "com.example.NonExistentBackupClass", "java.net.InetSocketAddress"};
			for (String className : invalidClassNames) {
				UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = className;
				assertThrows(IllegalStateException.class, ExternalBackup::getInstance);
			}
		} finally {
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = saved;
		}
	}

        @Test
        public void areExternalBackupsEnabledReturnsTrueWhenConfigured() {
                String saved = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
                try {
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = "com.example.SomeClass";
                        assertTrue(ExternalBackup.areExternalBackupsEnabled());
                } finally {
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = saved;
                }
        }

	@Test
	public void getInstanceCreatesConfiguredExternalBackupImplementation() {
		String saved = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		try {
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = TestExternalBackup.class.getName();
			assertTrue(ExternalBackup.getInstance() instanceof TestExternalBackup);
		}
		finally {
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = saved;
		}
	}

	public static class TestExternalBackup implements ExternalBackup {
		@Override
		public List<String> listBackups() {
			return List.of();
		}

		@Override
		public boolean exists(String backupName) {
			return false;
		}

		@Override
		public void downloadBackup(String backupName, OutputStream outputStream) {
			// test stub
		}

		@Override
		public void uploadBackup(String backupFilePath) {
			// test stub
		}

		@Override
		public void deleteBackup(String backupName) {
			// test stub
		}

		@Override
		public void copyBackup(String srcBackupName, String destBackupName) {
			// test stub
		}

		@Override
		public void moveBackup(String srcBackupName, String destBackupName) {
			// test stub
		}

		@Override
		public long getFileSize(String fileName) {
			return 0L;
		}
	}
}
