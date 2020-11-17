package org.skyve.impl.backup;

import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobItem;
import com.azure.storage.blob.sas.BlobContainerSasPermission;
import com.azure.storage.blob.sas.BlobServiceSasSignatureValues;

import java.io.OutputStream;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
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

	@Override
	public void copyBackup(String srcBackupName, String destBackupName) {
		final BlobContainerSasPermission permission = new BlobContainerSasPermission();
		permission.setReadPermission(true);
		final String sas = getBlobContainerClient().generateSas(new BlobServiceSasSignatureValues(OffsetDateTime.now().plusMinutes(10), permission));
		final String srcBlobUrl = getBlobContainerClient().getBlobClient(srcBackupName).getBlobUrl();
		getBlobContainerClient().getBlobClient(destBackupName).copyFromUrl(srcBlobUrl + "?" + sas);
	}

	@Override
	public void moveBackup(String srcBackupName, String destBackupName) {
		copyBackup(srcBackupName, destBackupName);
		deleteBackup(srcBackupName);
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
