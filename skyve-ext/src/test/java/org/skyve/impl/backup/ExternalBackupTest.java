package org.skyve.impl.backup;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;

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
	public void getInstanceThrowsWhenNotConfigured() {
		// BACKUP_EXTERNAL_BACKUP_CLASS is null by default
		String saved = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
		try {
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = null;
			assertThrows(IllegalStateException.class, ExternalBackup::getInstance);
		} finally {
			UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = saved;
		}
	}

        @Test
        public void getInstanceThrowsWhenClassNotFound() {
                // covers the try block path (L24-27): class name is set but class doesn't exist
                String saved = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
                try {
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = "com.example.NonExistentBackupClass";
                        assertThrows(IllegalStateException.class, ExternalBackup::getInstance);
                } finally {
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = saved;
                }
        }

        @Test
        public void getInstanceThrowsInstantiationException() {
                // covers L24-25: class exists but has no no-arg constructor, so newInstance throws NoSuchMethodException
                String saved = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
                try {
                        // InetSocketAddress has no no-arg constructor → NoSuchMethodException → IllegalStateException
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = "java.net.InetSocketAddress";
                        assertThrows(IllegalStateException.class, ExternalBackup::getInstance);
                } finally {
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = saved;
                }
        }

        @Test
        public void areExternalBackupsEnabledReturnsTrueWhenConfigured() {
                String saved = UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS;
                try {
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = "com.example.SomeClass";
                        org.junit.Assert.assertTrue(ExternalBackup.areExternalBackupsEnabled());
                } finally {
                        UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS = saved;
                }
        }
}
