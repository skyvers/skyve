package modules.admin.DataMaintenance.actions;

import java.io.File;

import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
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
			delete(backupDir);
		}

		bean.setSelectedBackupTimestampFolderName(null); // deselect the deleted backup
		bean.setRefreshBackups(Boolean.TRUE);

		return new ServerSideActionResult(bean);
	}
	
	private void delete(File f) throws ValidationException {
		if (f.isDirectory()) {
			for (File c : f.listFiles()) {
				delete(c);
			}
		}
		
		if (! f.delete()) {
			throw new ValidationException(new Message(f.getAbsolutePath() + " was not deleted"));
		}
	}
}
