package org.skyve.impl.backup;

import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobItem;

import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public class AzureBlobStorageBackup implements ExternalBackup {

	@Override
	public Set<String> listBackups() {
		return getBlobContainerClient().listBlobs().stream().map(BlobItem::getName).collect(Collectors.toSet());
	}

	@Override
	public boolean exists(String backupName) {
		return getBlobContainerClient().listBlobs().stream().anyMatch(blob -> Objects.equals(backupName, blob.getName()));
	}

	@Override
	public void downloadBackup(String backupName, OutputStream outputStream) {
		getBlobContainerClient().getBlobClient(backupName).download(outputStream);
	}

	@Override
	public void uploadBackup(String backupFilePath) {
		getBlobContainerClient().getBlobClient(Paths.get(backupFilePath).getFileName().toString()).uploadFromFile(backupFilePath);
	}

	@Override
	public void deleteBackup(String backupName) {
		getBlobContainerClient().getBlobClient(backupName).delete();
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

	private String getConnectionString() {
		final String connectionString = (String) ExternalBackup.getProperties().get("connectionString");
		if (connectionString == null) {
			throw new IllegalStateException("Missing JSON property connectionString under backup.");
		}
		return connectionString;
	}

	private String getContainerName() {
		final String containerName = (String) ExternalBackup.getProperties().get("containerName");
		if (containerName == null) {
			throw new IllegalStateException("Missing JSON property containerName under backup.");
		}
		return containerName;
	}

}
