package modules.admin.DataMaintenance.actions;

import java.io.File;
import java.io.FileOutputStream;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.Util;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import modules.admin.domain.DataMaintenance;

/**
 * Streams the selected backup file to the client.
 */
public class DownloadBackup extends DownloadAction<DataMaintenance> {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(DownloadBackup.class);

	/**
	 * Performs the prepare operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @throws Exception if the operation fails
	 */
	@Override
	public void prepare(DataMaintenance bean, WebContext webContext) throws Exception {
		String selectedBackupName = bean.getSelectedBackupName();
		final boolean backupExists;
		if (ExternalBackup.areExternalBackupsEnabled()) {
			backupExists = ExternalBackup.getInstance().exists(selectedBackupName);
		}
		else {
			File backup = new File(String.format("%sbackup_%s%s%s",
													Util.getBackupDirectory(),
													CORE.getUser().getCustomerName(),
													File.separator,
													selectedBackupName));
			backupExists = backup.exists();
			if (! backup.exists()) {
				LOGGER.warn("Backup {} DNE", backup.getAbsolutePath());
			}
		}

		if (! backupExists) {
			throw new ValidationException(new Message("Backup " + selectedBackupName + " no longer exists"));
		}
	}

	/**
	 * Performs the download operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public Download download(DataMaintenance bean, WebContext webContext)
	throws Exception {
		String selectedBackupName = bean.getSelectedBackupName();
		final File backup = new File(String.format("%sbackup_%s%s%s",
													Util.getBackupDirectory(),
													CORE.getUser().getCustomerName(),
													File.separator,
													selectedBackupName));

		if (ExternalBackup.areExternalBackupsEnabled()) {
			try (final FileOutputStream backupOutputStream = new FileOutputStream(backup)) {
				ExternalBackup.getInstance().downloadBackup(selectedBackupName, backupOutputStream);
			}
		}

		return new Download(selectedBackupName, backup, MimeType.zip);
	}
}
