package org.skyve.impl.backup;

import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

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
	 * @return The appropriate implementation instance of this interface as defined in the JSON configuration.
	 */
	@SuppressWarnings("unchecked")
	static ExternalBackup getInstance() {
		if (UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS != null) {
			try {
				Class<?> instanceClass = Thread.currentThread().getContextClassLoader().loadClass(UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS);
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
		return UtilImpl.BACKUP_EXTERNAL_BACKUP_CLASS != null;
	}

	/**
	 * @return A list of all the backups sorted by modified time descending.
	 */
	List<String> listBackups();

	/**
	 * Returns whether a complete, usable backup with the given name exists.
	 * <p>
	 * The scheduled backup job relies on this to decide whether a weekly, monthly or
	 * yearly backup has already been made for the current period, so an implementation
	 * must not report a partial or failed upload or copy as existing.
	 *
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
	 * Copies a backup to a new name and returns only once the destination is complete.
	 * <p>
	 * On failure an implementation must throw and must not leave a partial destination
	 * behind, otherwise {@link #exists(String)} would stop the scheduled backup job from
	 * retrying the copy for the rest of the period.
	 *
	 * @param srcBackupName The name of the backup to copy.
	 * @param destBackupName The name of the backup to copy to.
	 */
	void copyBackup(String srcBackupName, String destBackupName);

	/**
	 * Moves a backup to a new name and returns only once the destination is complete.
	 * The same contract as {@link #copyBackup(String, String)} applies.
	 *
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
