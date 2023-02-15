package org.skyve.impl.backup;

import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Map;

import org.skyve.impl.util.UtilImpl;

/**
 * An interface that defines common backup functionality and can be used to implement a variety of backup strategies
 * including offsite cloud storage.
 *
 * The desired implementation that should be used needs to be defined under "externalBackupClass" key under "backups"
 * within the JSON configuration of the Skyve application.
 */
public interface ExternalBackup {

	/**
	 * @return The backup properties defined in the JSON configuration.
	 */
	@SuppressWarnings("unchecked")
	static Map<String, Object> getProperties() {
		if (UtilImpl.CONFIGURATION != null) {
			return (Map<String, Object>) UtilImpl.CONFIGURATION.get("backup");
		}

		return null;
	}

	/**
	 * @return The classname of the implementation instance of this interface as defined in the JSON configuration.
	 */
	static String getExternalBackupClass() {
		Map<String, Object> backupProperties = getProperties();
		if (backupProperties != null) {
			return (String) backupProperties.get("externalBackupClass");
		}

		return null;
	}

	/**
	 * @return The appropriate implementation instance of this interface as defined in the JSON configuration.
	 */
	@SuppressWarnings("unchecked")
	static ExternalBackup getInstance() {
		final String externalBackupClass = getExternalBackupClass();
		if (externalBackupClass != null) {
			try {
				Class<?> instanceClass = Class.forName(externalBackupClass);
				return ((Class<? extends ExternalBackup>) instanceClass).getDeclaredConstructor().newInstance();
			} catch (InstantiationException | IllegalAccessException | ClassNotFoundException | InvocationTargetException | NoSuchMethodException e) {
				throw new IllegalStateException("Failed to instantiate external backup class.", e);
			}
		}
		throw new IllegalStateException(
				"Unable to instantiate ExternalBackup instance as the externalBackupClass property has not been configured in the JSON configuration.");
	}

	/**
	 * @return True if there is an externalBackupClass defined in the JSON configuration, false otherwise.
	 */
	static boolean areExternalBackupsEnabled() {
		return getExternalBackupClass() != null;
	}

	/**
	 * @return A list of all the backups sorted by modified time descending.
	 */
	List<String> listBackups();

	/**
	 * @param backupName The name of the backup to check the existence for.
	 * @return True if the given backup exists.
	 */
	boolean exists(String backupName);

	/**
	 * @param backupName The name of the backup to download.
	 * @param outputStream The output stream to write the backup to.
	 */
	void downloadBackup(String backupName, OutputStream outputStream);

	/**
	 * @param backupFilePath The full path to the backup that is to be uploaded.
	 */
	void uploadBackup(String backupFilePath);

	/**
	 * @param backupName The name of the backup to delete.
	 */
	void deleteBackup(String backupName);

	/**
	 * @param srcBackupName The name of the backup to copy.
	 * @param destBackupName The name of the backup to copy to.
	 */
	void copyBackup(String srcBackupName, String destBackupName);

	/**
	 * @param srcBackupName The name of the backup to move.
	 * @param destBackupName The name of the backup to move to.
	 */
	void moveBackup(String srcBackupName, String destBackupName);

	/**
	 * @param fileName The name of the backup.
	 * @return The size of the backup in bytes.
	 */
	long getFileSize(String fileName);
}
