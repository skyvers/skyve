package org.skyve.impl.backup;

import java.io.OutputStream;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;

import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobItem;
import com.azure.storage.blob.models.BlobProperties;
import com.azure.storage.blob.sas.BlobContainerSasPermission;
import com.azure.storage.blob.sas.BlobServiceSasSignatureValues;

public class AzureBlobStorageBackup implements ExternalBackup {
	public static final String AZURE_CONNECTION_STRING_KEY = "connectionString";
	public static final String AZURE_CONTAINER_NAME_KEY = "containerName";

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
		getBlobContainerClient().getBlobClient(getDirectoryName() + backupName).download(outputStream);
	}

	@Override
	public void uploadBackup(String backupFilePath) {
		getBlobContainerClient().getBlobClient(getDirectoryName() + Paths.get(backupFilePath).getFileName().toString())
				.uploadFromFile(backupFilePath);
	}

	@Override
	public void deleteBackup(String backupName) {
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
		final BlobContainerSasPermission permission = new BlobContainerSasPermission();
		permission.setReadPermission(true);
		final String sas = getBlobContainerClient().generateSas(new BlobServiceSasSignatureValues(OffsetDateTime.now().plusMinutes(10), permission));
		final String srcBlobUrl = getBlobContainerClient().getBlobClient(getDirectoryName() + srcBackupName).getBlobUrl();
		getBlobContainerClient().getBlobClient(getDirectoryName() + destBackupName).copyFromUrl(srcBlobUrl + "?" + sas);
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
