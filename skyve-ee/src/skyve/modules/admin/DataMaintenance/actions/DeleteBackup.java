package modules.admin.DataMaintenance.actions;

import java.io.File;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class DeleteBackup implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 5306067916641877356L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		File backup = new File(String.format("%sbackup_%s%s%s", 
												Util.getContentDirectory(),
												customerName,
												File.separator,
												bean.getSelectedBackupName()));
		if (backup.exists()) {
			Util.LOGGER.info("Deleting backup " + backup.getAbsolutePath());
			FileUtil.delete(backup);
			Util.LOGGER.info("Deleted backup " + backup.getAbsolutePath());
		}
		else {
			Util.LOGGER.info("Backup " + backup.getAbsolutePath() + " no longer exists");
		}

		bean.setSelectedBackupName(null); // deselect the deleted backup
		bean.setRefreshBackups(Boolean.TRUE);

		return new ServerSideActionResult<>(bean);
	}
}
