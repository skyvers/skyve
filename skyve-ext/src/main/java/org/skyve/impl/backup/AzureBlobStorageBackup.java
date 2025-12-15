package org.skyve.impl.backup;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobItem;
import com.azure.storage.blob.models.BlobProperties;
import com.azure.storage.blob.models.BlobRequestConditions;
import com.azure.storage.blob.models.ParallelTransferOptions;
import com.azure.storage.blob.specialized.BlobOutputStream;

public class AzureBlobStorageBackup implements ExternalBackup {

    private static final Logger LOGGER = LoggerFactory.getLogger(AzureBlobStorageBackup.class);

	public static final String AZURE_CONNECTION_STRING_KEY = "connectionString";
	public static final String AZURE_CONTAINER_NAME_KEY = "containerName";
	private static final long BLOCK_SIZE = 4194304; // 4 MB

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

	@Override
	public void copyBackup(String srcBackupName, String destBackupName) {
		BlobClient srcBlobClient = getBlobContainerClient().getBlobClient(getDirectoryName() + srcBackupName);

		try {
			BlobClient destBlobClient = getBlobContainerClient().getBlobClient(getDirectoryName() + destBackupName);
			LOGGER.info("Copying from {} to {} in Azure", srcBackupName, destBackupName);

			// copy the backup in chunks to workaround Azure's blob size limit
			ParallelTransferOptions opts = new ParallelTransferOptions()
					.setBlockSizeLong(Long.valueOf(BLOCK_SIZE))
					.setMaxConcurrency(Integer.valueOf(5));

			BlobRequestConditions requestConditions = new BlobRequestConditions();

			try (BlobOutputStream bos = destBlobClient.getBlockBlobClient()
					.getBlobOutputStream(
							opts, null, null, null, requestConditions);

					InputStream is = srcBlobClient.getBlockBlobClient().openInputStream()) {

				byte[] buffer = new byte[(int) BLOCK_SIZE];
				for (int len; (len = is.read(buffer)) != -1;) {
					bos.write(buffer, 0, len);
				}
			}

			LOGGER.info("Successfully copied to {} in Azure", destBackupName);
		} catch (IOException e) {
			throw new DomainException(e);
		}
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
