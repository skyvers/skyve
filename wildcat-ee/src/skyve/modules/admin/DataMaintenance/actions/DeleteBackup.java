package modules.admin.DataMaintenance.actions;

import java.io.File;

import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;
import org.skyve.wildcat.util.UtilImpl;

public class DeleteBackup implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 5306067916641877356L;

	@Override
	public ServerSideActionResult execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		File backupDir = new File(UtilImpl.CONTENT_DIRECTORY + "backup_" + customerName + File.separator + bean.getSelectedBackupTimestampFolderName());
		if (backupDir.exists() && backupDir.isDirectory()) {
			if  (! backupDir.delete()) {
				throw new IllegalStateException(backupDir.getAbsolutePath() + " was not deleted");
			}
		}

		bean.setSelectedBackupTimestampFolderName(null); // deselect the deleted backup
		return new ServerSideActionResult(bean);
	}
}
