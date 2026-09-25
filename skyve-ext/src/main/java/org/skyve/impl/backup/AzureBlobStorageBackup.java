package org.skyve.impl.backup;

import java.io.OutputStream;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import com.azure.core.util.polling.LongRunningOperationStatus;
import com.azure.core.util.polling.PollResponse;
import com.azure.core.util.polling.SyncPoller;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobCopyInfo;
import com.azure.storage.blob.models.BlobItem;
import com.azure.storage.blob.models.BlobProperties;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * {@link ExternalBackup} implementation that stores Skyve backup archives in
 * Azure Blob Storage.
 *
 * <p>Configured via the {@code backups.externalBackupClass} JSON key with
 * {@code connectionString} and {@code containerName} sub-keys.
 */
public class AzureBlobStorageBackup implements ExternalBackup {

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(AzureBlobStorageBackup.class);

	public static final String AZURE_CONNECTION_STRING_KEY = "connectionString";
	public static final String AZURE_CONTAINER_NAME_KEY = "containerName";
	private static final String AZURE_SAS_CONNECTION_STRING_KEY = "SharedAccessSignature";
	private static final long COPY_POLL_INTERVAL_SECONDS = 2;
	private static final Duration COPY_TIMEOUT = Duration.ofHours(4);

	@Override
	public List<String> listBackups() {
		final Comparator<BlobItem> byLastModifiedDate = Comparator.comparing(blobItem -> blobItem.getProperties().getLastModified());
		return getBlobContainerClient().listBlobsByHierarchy(getDirectoryName()).stream()
				.sorted(byLastModifiedDate.reversed())
				.map(b -> b.getName().replace(getDirectoryName(), ""))
				.collect(Collectors.toList());
	}

	@Override
	public boolean exists(String backupName) {
		return getBlobContainerClient().listBlobsByHierarchy(getDirectoryName()).stream()
				.anyMatch(blob -> Objects.equals(backupName, blob.getName().replace(getDirectoryName(), "")));
	}

	@Override
	public void downloadBackup(String backupName, OutputStream outputStream) {
		LOGGER.info("Downloading backup {} from Azure", backupName);
		getBlobClient(backupName).downloadStream(outputStream);
	}

	@Override
	public void uploadBackup(String backupFilePath) {
		LOGGER.info("Uploading backup {} to Azure", Paths.get(backupFilePath).getFileName().toString());
		getBlobClient(Paths.get(backupFilePath).getFileName().toString()).uploadFromFile(backupFilePath);
	}

	@Override
	public void deleteBackup(String backupName) {
		LOGGER.info("Deleting backup {} from Azure", backupName);
		getBlobClient(backupName).delete();
	}

	private BlobContainerClient getBlobContainerClient() {
		final BlobServiceClient blobServiceClient = new BlobServiceClientBuilder().connectionString(getConnectionString()).buildClient();
		final BlobContainerClient blobContainerClient = blobServiceClient.getBlobContainerClient(getContainerName());

		// Ensure only one thread will ever create the blob container.
		synchronized (this) {
			if (!blobContainerClient.exists()) {
				blobContainerClient.create();
			}
		}

		return blobContainerClient;
	}

	/** Returns the client for {@code backupName} in this customer's backup directory. */
	private @Nonnull BlobClient getBlobClient(@Nonnull String backupName) {
		return getBlobContainerClient().getBlobClient(getDirectoryName() + backupName);
	}

	/**
	 * Copies a backup blob to a new name using Azure's asynchronous server-side copy
	 * and returns once the destination blob is complete.
	 *
	 * <p>The copy runs inside the storage account, so the backup is never streamed
	 * through the application server (no egress charge, no local bandwidth) and there
	 * is no blob size limit, unlike the synchronous copy-from-URL operation which is
	 * capped at 256 MB. With an account-key connection string the destination's
	 * Shared Key authorisation is applied to the same-account source. With a SAS
	 * connection string Azure does not extend the request's SAS to the source, so the
	 * connection string's SAS is appended to the source URL.
	 *
	 * <p>Side effects: creates the destination blob. If the copy fails, is aborted or
	 * does not complete within {@link #COPY_TIMEOUT}, any partial destination blob is
	 * deleted before throwing so a later retry is not mistaken for a finished copy.
	 *
	 * @throws DomainException if the copy does not complete successfully
	 */
	@Override
	public void copyBackup(String srcBackupName, String destBackupName) {
		// resolve the container (a client build plus an existence round-trip) once for both blobs
		final BlobContainerClient blobContainerClient = getBlobContainerClient();
		final String directoryName = getDirectoryName();
		copy(srcBackupName,
				destBackupName,
				copySourceUrl(blobContainerClient.getBlobClient(directoryName + srcBackupName).getBlobUrl()),
				blobContainerClient.getBlobClient(directoryName + destBackupName));
	}

	/**
	 * Returns the URL Azure should read the copy source from: the blob URL, plus the
	 * connection string's shared access signature when the connection string carries one.
	 */
	private static String copySourceUrl(String srcBlobUrl) {
		final String sas = getSharedAccessSignature();
		return (sas == null) ? srcBlobUrl : srcBlobUrl + '?' + sas;
	}

	/**
	 * Performs the server-side copy from a source URL to a resolved blob client; see {@link #copyBackup(String, String)}.
	 * Kept separate from {@link #copyBackup(String, String)} by agreement so the copy logic can be
	 * exercised with a mocked blob client without a protected seam.
	 */
	private static void copy(String srcBackupName, String destBackupName, String srcUrl, BlobClient destBlobClient) {
		LOGGER.info("Copying from {} to {} in Azure", srcBackupName, destBackupName);

		final SyncPoller<BlobCopyInfo, Void> poller = destBlobClient.beginCopy(srcUrl,
																				Duration.ofSeconds(COPY_POLL_INTERVAL_SECONDS));
		final PollResponse<BlobCopyInfo> response;
		try {
			response = poller.waitForCompletion(COPY_TIMEOUT);
		}
		catch (RuntimeException e) {
			abandonCopy(poller, destBlobClient, destBackupName);
			throw new DomainException(String.format("Failed to copy %s to %s in Azure - copy did not complete within %s",
													srcBackupName,
													destBackupName,
													COPY_TIMEOUT),
										e);
		}

		if (! LongRunningOperationStatus.SUCCESSFULLY_COMPLETED.equals(response.getStatus())) {
			final BlobCopyInfo info = response.getValue();
			destBlobClient.deleteIfExists();
			throw new DomainException(String.format("Failed to copy %s to %s in Azure - copy status %s%s",
													srcBackupName,
													destBackupName,
													(info == null) ? response.getStatus() : info.getCopyStatus(),
													((info == null) || (info.getError() == null)) ? "" : " - " + info.getError()));
		}

		LOGGER.info("Successfully copied to {} in Azure", destBackupName);
	}

	/**
	 * Aborts an in-flight copy and removes whatever partial destination blob it left behind.
	 * Failures here are logged and swallowed because the caller is already about to throw.
	 */
	private static void abandonCopy(SyncPoller<BlobCopyInfo, Void> poller, BlobClient destBlobClient, String destBackupName) {
		try {
			poller.cancelOperation();
		}
		catch (RuntimeException e) {
			LOGGER.warn("Could not abort the Azure copy to {}", destBackupName, e);
		}
		try {
			destBlobClient.deleteIfExists();
		}
		catch (RuntimeException e) {
			LOGGER.warn("Could not delete the partial Azure copy {}", destBackupName, e);
		}
	}

	@Override
	public void moveBackup(String srcBackupName, String destBackupName) {
		copyBackup(srcBackupName, destBackupName);
		deleteBackup(srcBackupName);
	}

	@Override
	public long getFileSize(String fileName) {
		final BlobProperties blobProperties = getBlobClient(fileName).getProperties();
		long result = blobProperties.getBlobSize();

		return result;
	}

	private static String getConnectionString() {
		final String connectionString = (String) UtilImpl.BACKUP_PROPERTIES.get(AZURE_CONNECTION_STRING_KEY);
		if (connectionString == null) {
			throw new IllegalStateException("Missing JSON property connectionString under backup.");
		}
		return connectionString;
	}

	/**
	 * Returns the shared access signature from the configured connection string without a
	 * leading question mark, or {@code null} when the connection string uses an account key.
	 */
	private static @Nullable String getSharedAccessSignature() {
		for (String pair : getConnectionString().split(";")) {
			final int equals = pair.indexOf('=');
			if ((equals > 0) && AZURE_SAS_CONNECTION_STRING_KEY.equalsIgnoreCase(pair.substring(0, equals).trim())) {
				final String sas = pair.substring(equals + 1).trim();
				return sas.startsWith("?") ? sas.substring(1) : sas;
			}
		}
		return null;
	}

	private static String getContainerName() {
		final String containerName = (String) UtilImpl.BACKUP_PROPERTIES.get(AZURE_CONTAINER_NAME_KEY);
		if (containerName == null) {
			throw new IllegalStateException("Missing JSON property containerName under backup.");
		}
		return containerName;
	}

	private static String getDirectoryName() {
		return "backup-" + CORE.getCustomer().getName().toLowerCase() + "/";
	}
	
}
