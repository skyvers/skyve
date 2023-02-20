package modules.admin.DataMaintenance.actions;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.DataMaintenance.DataMaintenanceExtension;
import modules.admin.domain.DataMaintenance;

public class RenameBackup implements ServerSideAction<DataMaintenance> {
	
	private final String ZIP_SUFFIX = ".zip";

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		if (bean.getNewBackupName() == null) {
			throw new ValidationException(
					new Message(DataMaintenance.newBackupNamePropertyName, "New Backup name must be entered."));
		}
		if (bean.getNewBackupName().endsWith(ZIP_SUFFIX)) {
			throw new ValidationException(
					new Message(DataMaintenance.newBackupNamePropertyName,
							"New Backup name does not need .zip appended, this will occur automatically when renamed."));
		}

		if (ExternalBackup.areExternalBackupsEnabled()) {
			ExternalBackup.getInstance().moveBackup(bean.getSelectedBackupName(), bean.getNewBackupName());
		} else {
			String directoryPrefix = DataMaintenanceExtension.backupDirectoryPrefix();
			Path fromFile = Path.of(directoryPrefix + File.separator + bean.getSelectedBackupName());
			Path toFile = Path.of(directoryPrefix + File.separator + bean.getNewBackupName() + ZIP_SUFFIX);

			Files.move(fromFile, toFile, StandardCopyOption.ATOMIC_MOVE);
		}
		bean.setNewBackupName(null);

		return new ServerSideActionResult<>(bean);
	}
}
