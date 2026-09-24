package org.skyve.impl.backup;

import java.io.OutputStream;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.OffsetDateTime;
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
import com.azure.storage.blob.sas.BlobSasPermission;
import com.azure.storage.blob.sas.BlobServiceSasSignatureValues;

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
	private static final long COPY_SAS_EXPIRY_MINUTES = 60;
	private static final long COPY_POLL_INTERVAL_SECONDS = 2;

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
		getBlobContainerClient().getBlobClient(getDirectoryName() + backupName).downloadStream(outputStream);
	}

	@Override
	public void uploadBackup(String backupFilePath) {
		LOGGER.info("Uploading backup {} to Azure", Paths.get(backupFilePath).getFileName().toString());
		getBlobContainerClient().getBlobClient(getDirectoryName() + Paths.get(backupFilePath).getFileName().toString())
				.uploadFromFile(backupFilePath);
	}

	@Override
	public void deleteBackup(String backupName) {
		LOGGER.info("Deleting backup {} from Azure", backupName);
		getBlobContainerClient().getBlobClient(getDirectoryName() + backupName).delete();
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

	/**
	 * Copy a backup blob to a new name using Azure's server-side asynchronous copy.
	 * <p>
	 * The copy happens entirely within the storage account, so the backup is never
	 * streamed through the application server (no egress charge, no local bandwidth)
	 * and there is no blob size limit (unlike the synchronous copy-from-URL operation,
	 * which is capped at 256 MB).
	 * The source is addressed with a short-lived read SAS so the copy is authorised
	 * regardless of whether the account permits anonymous same-account copies.
	 */
	@Override
	public void copyBackup(String srcBackupName, String destBackupName) {
		final BlobContainerClient blobContainerClient = getBlobContainerClient();
		final BlobClient srcBlobClient = blobContainerClient.getBlobClient(getDirectoryName() + srcBackupName);
		final BlobClient destBlobClient = blobContainerClient.getBlobClient(getDirectoryName() + destBackupName);
		LOGGER.info("Copying from {} to {} in Azure", srcBackupName, destBackupName);

		final BlobSasPermission readPermission = new BlobSasPermission().setReadPermission(true);
		final String sas = srcBlobClient.generateSas(new BlobServiceSasSignatureValues(OffsetDateTime.now().plusMinutes(COPY_SAS_EXPIRY_MINUTES),
																						readPermission));
		final SyncPoller<BlobCopyInfo, Void> poller = destBlobClient.beginCopy(srcBlobClient.getBlobUrl() + "?" + sas,
																				Duration.ofSeconds(COPY_POLL_INTERVAL_SECONDS));
		final PollResponse<BlobCopyInfo> response = poller.waitForCompletion();
		final BlobCopyInfo info = response.getValue();
		if (! LongRunningOperationStatus.SUCCESSFULLY_COMPLETED.equals(response.getStatus())) {
			throw new DomainException(String.format("Failed to copy %s to %s in Azure - copy status %s%s",
													srcBackupName,
													destBackupName,
													(info == null) ? response.getStatus() : info.getCopyStatus(),
													((info == null) || (info.getError() == null)) ? "" : " - " + info.getError()));
		}

		LOGGER.info("Successfully copied to {} in Azure", destBackupName);
	}

	@Override
	public void moveBackup(String srcBackupName, String destBackupName) {
		copyBackup(srcBackupName, destBackupName);
		deleteBackup(srcBackupName);
	}

	@Override
	public long getFileSize(String fileName) {
		final BlobContainerClient blobContainerClient = getBlobContainerClient();
		final BlobClient blobClient = blobContainerClient.getBlobClient(getDirectoryName() + fileName);
		final BlobProperties blobProperties = blobClient.getProperties();
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
