package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import java.io.File;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class Backup implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -2943997026132660437L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshBackups(Boolean.TRUE);
		
		Util.LOGGER.info("Creating backup.");
		File backupDir = org.skyve.impl.backup.Backup.backup();
		Util.LOGGER.info("Created backup folder " + backupDir.getAbsolutePath());

		File zip = new File(backupDir.getParentFile(), backupDir.getName() + ".zip");
		FileUtil.createZipArchive(backupDir, zip);
		Util.LOGGER.info("Compressed backup to " + zip.getAbsolutePath());
		FileUtil.delete(backupDir);
		Util.LOGGER.info("Deleted backup folder " + backupDir.getAbsolutePath());
		
		return new ServerSideActionResult<>(bean);
	}
}
