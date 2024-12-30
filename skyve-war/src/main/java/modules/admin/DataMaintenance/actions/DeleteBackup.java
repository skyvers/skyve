package modules.admin.DataMaintenance.actions;

import java.io.File;

import org.skyve.CORE;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import modules.admin.domain.DataMaintenance;

public class DeleteBackup implements ServerSideAction<DataMaintenance> {

    private static final Logger LOGGER = LoggerFactory.getLogger(DeleteBackup.class);

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
			throws Exception {
		String customerName = CORE.getUser().getCustomerName();

		// delete external backup if enabled
		if (ExternalBackup.areExternalBackupsEnabled()) {
			String backupName = bean.getSelectedBackupName();
			if (ExternalBackup.getInstance().exists(backupName)) {
				LOGGER.info("Deleting backup " + backupName);
				ExternalBackup.getInstance().deleteBackup(bean.getSelectedBackupName());
				LOGGER.info("Deleted backup " + backupName);
			} else {
				LOGGER.info("Backup " + backupName + " no longer exists");
			}
		}
		// delete from local content
		File backup = new File(String.format("%sbackup_%s%s%s",
				Util.getBackupDirectory(),
				customerName,
				File.separator,
				bean.getSelectedBackupName()));
		if (backup.exists()) {
			LOGGER.info("Deleting backup " + backup.getAbsolutePath());
			FileUtil.delete(backup);
			LOGGER.info("Deleted backup " + backup.getAbsolutePath());
		} else {
			LOGGER.info("Backup " + backup.getAbsolutePath() + " no longer exists");
		}

		// deselect the deleted backup
		bean.setSelectedBackupName(null);
		bean.setRefreshBackups(Boolean.TRUE);

		return new ServerSideActionResult<>(bean);
	}
}
