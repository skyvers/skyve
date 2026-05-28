package modules.admin.DataMaintenance.actions;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.skyve.CORE;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.util.Util;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import modules.admin.domain.DataMaintenance;

/**
 * Stores an uploaded backup file in the configured backup location.
 */
public class UploadBackup extends UploadAction<DataMaintenance> {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(UploadBackup.class);

	/**
	 * Performs the upload operation.
	 * @param bean the bean value
	 * @param upload the upload value
	 * @param exception the exception value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public DataMaintenance upload(final DataMaintenance bean, final Upload upload,
			final UploadException exception, final WebContext webContext) throws Exception {

		// create the backup upload file
		File backup = new File(String.format("%sbackup_%s%s%s",
												Util.getBackupDirectory(),
												CORE.getUser().getCustomerName(),
												File.separator,
												upload.getFileName()));

		// check that a backup with this filename doesn't already exist
		if (backup.exists()) {
			exception.addError(new Problem(String.format("A backup with the name %s already exists.", upload.getFileName()), null));
			throw exception;
		}

		//check that the backup folder exists, and if not, create it
		File backup_directory = new File(String.format("%sbackup_%s",
														Util.getContentDirectory(),
														CORE.getUser().getCustomerName()));
		if (! backup_directory.exists()) {
	        backup_directory.mkdir();
		}

		// copy the input file to the backup location
		try (InputStream in = upload.getInputStream()) {
			Files.copy(in, Paths.get(backup.getAbsolutePath()));
		}
		if (backup.exists()) {
			LOGGER.info("Uploaded backup {}", backup.getAbsolutePath());
		}

		if (ExternalBackup.areExternalBackupsEnabled()) {
			ExternalBackup.getInstance().uploadBackup(backup.getAbsolutePath());
			if (! backup.delete()) {
				LOGGER.warn("Backup {} was successfully uploaded externally but could not be deleted on disk.", backup.getAbsolutePath());
			}
		}

		// refresh the list of backups to include the uploaded one
		bean.setRefreshBackups(Boolean.TRUE);

		return bean;
	}
}
