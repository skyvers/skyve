package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.Duration;
import java.util.HashMap;

import org.junit.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import com.azure.core.util.polling.LongRunningOperationStatus;
import com.azure.core.util.polling.PollResponse;
import com.azure.core.util.polling.SyncPoller;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.models.BlobCopyInfo;
import com.azure.storage.blob.models.CopyStatusType;

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
	public void copyStartsServerSideCopyFromTheSourceUrlAndWaitsForSuccess() throws Exception {
		CopyFixture fixture = new CopyFixture();
		when(fixture.poller.waitForCompletion(any(Duration.class)))
				.thenReturn(new PollResponse<>(LongRunningOperationStatus.SUCCESSFULLY_COMPLETED, copyInfo(CopyStatusType.SUCCESS, null)));

		invokeCopy(fixture);

		verify(fixture.dest).beginCopy(eq(SOURCE_URL), any(Duration.class));
		verify(fixture.dest, never()).deleteIfExists();
		verify(fixture.poller, never()).cancelOperation();
	}

	@Test
	public void copyDeletesPartialDestinationAndThrowsWhenTheCopyFails() {
		CopyFixture fixture = new CopyFixture();
		when(fixture.poller.waitForCompletion(any(Duration.class)))
				.thenReturn(new PollResponse<>(LongRunningOperationStatus.FAILED, copyInfo(CopyStatusType.FAILED, "server busy")));

		DomainException thrown = assertCopyThrows(fixture);

		assertThat(thrown.getMessage(), containsString("Failed to copy source.zip to destination.zip in Azure"));
		assertThat(thrown.getMessage(), containsString("copy status failed - server busy"));
		verify(fixture.dest).deleteIfExists();
		verify(fixture.poller, never()).cancelOperation();
	}

	@Test
	public void copyAbortsAndDeletesPartialDestinationWhenTheCopyTimesOut() {
		CopyFixture fixture = new CopyFixture();
		when(fixture.poller.waitForCompletion(any(Duration.class))).thenThrow(new IllegalStateException("timed out"));

		DomainException thrown = assertCopyThrows(fixture);

		assertThat(thrown.getMessage(), containsString("copy did not complete within PT4H"));
		assertEquals("timed out", thrown.getCause().getMessage());
		verify(fixture.poller).cancelOperation();
		verify(fixture.dest).deleteIfExists();
	}

	@Test
	public void copySourceUrlIsThePlainBlobUrlForAnAccountKeyConnectionString() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY,
				"DefaultEndpointsProtocol=https;AccountName=account;AccountKey=c2VjcmV0;EndpointSuffix=core.windows.net");

		withBackupProperties(properties, () -> assertEquals(SOURCE_URL, invokeCopySourceUrl()));
	}

	@Test
	public void copySourceUrlCarriesTheSharedAccessSignatureFromTheConnectionString() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY,
				"BlobEndpoint=https://account.blob.core.windows.net/;SharedAccessSignature=sv=2024-11-04&ss=b&srt=sco&sp=rwdlac&sig=abc%2Fdef%3D");

		withBackupProperties(properties, () -> assertEquals(SOURCE_URL + "?sv=2024-11-04&ss=b&srt=sco&sp=rwdlac&sig=abc%2Fdef%3D", invokeCopySourceUrl()));
	}

	@Test
	public void copySourceUrlIgnoresASharedAccessSignatureKeyTheSdkWouldNotMatch() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY,
				"AccountName=account;AccountKey=c2VjcmV0;sharedaccesssignature=sv=2024-11-04&sig=abc");

		withBackupProperties(properties, () -> assertEquals(SOURCE_URL, invokeCopySourceUrl()));
	}

	@Test
	public void copySourceUrlUsesTheLastSharedAccessSignatureLikeTheSdk() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY,
				"BlobEndpoint=https://account.blob.core.windows.net/;SharedAccessSignature=sig=first;SharedAccessSignature=sig=second");

		withBackupProperties(properties, () -> assertEquals(SOURCE_URL + "?sig=second", invokeCopySourceUrl()));
	}

	@Test
	public void copySourceUrlStripsALeadingQuestionMarkFromTheSharedAccessSignature() throws Exception {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(AzureBlobStorageBackup.AZURE_CONNECTION_STRING_KEY,
				"BlobEndpoint=https://account.blob.core.windows.net/;SharedAccessSignature=?sv=2024-11-04&sig=abc");

		withBackupProperties(properties, () -> assertEquals(SOURCE_URL + "?sv=2024-11-04&sig=abc", invokeCopySourceUrl()));
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

	private static void assertPrivateConfigThrows(String methodName) {
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

	private static final String SOURCE_URL = "https://account.blob.core.windows.net/container/backup-acme/source.zip";

	private static BlobCopyInfo copyInfo(CopyStatusType status, String error) {
		return new BlobCopyInfo(SOURCE_URL, "copy-id", status, "etag", null, error);
	}

	/**
	 * Invokes the private static copy method with a mocked destination blob client so nothing talks to Azure.
	 */
	private static void invokeCopy(CopyFixture fixture) throws Exception {
		Method method = AzureBlobStorageBackup.class.getDeclaredMethod("copy", String.class, String.class, String.class, BlobClient.class);
		method.setAccessible(true);
		method.invoke(null, "source.zip", "destination.zip", SOURCE_URL, fixture.dest);
	}

	private static Object invokeCopySourceUrl() throws Exception {
		Method method = AzureBlobStorageBackup.class.getDeclaredMethod("copySourceUrl", String.class);
		method.setAccessible(true);
		return method.invoke(null, SOURCE_URL);
	}

	private static DomainException assertCopyThrows(CopyFixture fixture) {
		InvocationTargetException thrown = assertThrows(InvocationTargetException.class, () -> invokeCopy(fixture));
		assertEquals(DomainException.class, thrown.getCause().getClass());
		return (DomainException) thrown.getCause();
	}

	private static final class CopyFixture {
		private final BlobClient dest = mock(BlobClient.class);
		@SuppressWarnings("unchecked")
		private final SyncPoller<BlobCopyInfo, Void> poller = mock(SyncPoller.class);

		private CopyFixture() {
			when(dest.beginCopy(any(String.class), any(Duration.class))).thenReturn(poller);
		}
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
