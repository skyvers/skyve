package modules.admin.DataMaintenance.actions;

import java.io.File;
import java.io.FileInputStream;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class DownloadBackup extends DownloadAction<DataMaintenance> {
	private static final long serialVersionUID = 4544317770456317465L;

	@Override
	@SuppressWarnings("resource")
	public Download download(DataMaintenance bean, WebContext webContext)
	throws Exception {
		String selectedBackupName = bean.getSelectedBackupName();
		File backup = new File(String.format("%sbackup_%s%s%s", 
												Util.getContentDirectory(),
												CORE.getUser().getCustomerName(),
												File.separator,
												selectedBackupName));
		if (! backup.exists()) {
			Util.LOGGER.warning("Backup " + backup.getAbsolutePath() + " DNE");
			throw new ValidationException(new Message("Backup " + selectedBackupName + " no longer exists"));
		}
		
		return new Download(selectedBackupName, new FileInputStream(backup), MimeType.zip);
	}
}
