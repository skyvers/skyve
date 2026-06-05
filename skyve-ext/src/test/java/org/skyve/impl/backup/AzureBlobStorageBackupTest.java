package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;

import org.junit.Test;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
public class AzureBlobStorageBackupTest {

	@Test
	public void getConnectionStringThrowsWhenConfigurationIsMissing() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		withBackupProperties(properties, () -> assertPrivateConfigThrows("getConnectionString"));
	}

	@Test
	public void getContainerNameThrowsWhenConfigurationIsMissing() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		withBackupProperties(properties, () -> assertPrivateConfigThrows("getContainerName"));
	}

	@Test
	public void privateConfigAccessorsReturnConfiguredValues() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY, "UseDevelopmentStorage=true");
		properties.put(AzureBlobStorageBackup.AZURE_CONTAINER_NAME_KEY, "skyve-backups");

		withBackupProperties(properties, () -> {
			assertEquals("UseDevelopmentStorage=true", invokePrivateConfig("getConnectionString"));
			assertEquals("skyve-backups", invokePrivateConfig("getContainerName"));
		});
	}

	private static void assertPrivateConfigThrows(String methodName) throws Exception {
		InvocationTargetException thrown = assertThrows(InvocationTargetException.class, () -> invokePrivateConfig(methodName));
		assertEquals(IllegalStateException.class, thrown.getCause().getClass());
	}

	private static Object invokePrivateConfig(String methodName) throws Exception {
		Method method = AzureBlobStorageBackup.class.getDeclaredMethod(methodName);
		method.setAccessible(true);
		return method.invoke(null);
	}

	private static void withBackupProperties(HashMap<String, Object> properties, ThrowingRunnable runnable) throws Exception {
		var saved = UtilImpl.BACKUP_PROPERTIES;
		try {
			UtilImpl.BACKUP_PROPERTIES = properties;
			runnable.run();
		}
		finally {
			UtilImpl.BACKUP_PROPERTIES = saved;
		}
	}

	private interface ThrowingRunnable {
		void run() throws Exception;
	}
}
