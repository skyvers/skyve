package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;

import org.junit.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
public class AzureBlobStorageBackupTest {

	@Test
	public void constructorCreatesInstance() {
		AzureBlobStorageBackup backup = new AzureBlobStorageBackup();
		assertNotNull(backup);
	}

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

	@Test
	public void moveBackupCopiesThenDeletesSource() {
		RecordingAzureBlobStorageBackup backup = new RecordingAzureBlobStorageBackup();

		backup.moveBackup("source.zip", "destination.zip");

		assertEquals("source.zip", backup.copiedSource);
		assertEquals("destination.zip", backup.copiedDestination);
		assertEquals("source.zip", backup.deletedBackup);
	}

	@Test
	public void getDirectoryNameUsesLowerCaseCustomerName() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("AcmeCorp");

		withThreadLocalPersistence(persistence, () -> assertEquals("backup-acmecorp/", invokePrivateConfig("getDirectoryName")));
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

	private static void withThreadLocalPersistence(AbstractPersistence persistence, ThrowingRunnable runnable) throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence previous = threadLocal.get();
		try {
			threadLocal.set(persistence);
			runnable.run();
		}
		finally {
			if (previous == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(previous);
			}
		}
	}

	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		return threadLocal;
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

	private static final class RecordingAzureBlobStorageBackup extends AzureBlobStorageBackup {
		private String copiedSource;
		private String copiedDestination;
		private String deletedBackup;

		@Override
		public void copyBackup(String srcBackupName, String destBackupName) {
			this.copiedSource = srcBackupName;
			this.copiedDestination = destBackupName;
		}

		@Override
		public void deleteBackup(String backupName) {
			this.deletedBackup = backupName;
		}
	}
}
