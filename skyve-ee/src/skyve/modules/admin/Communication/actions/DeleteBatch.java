package modules.admin.Communication.actions;

import java.io.File;

import modules.admin.domain.Communication;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class DeleteBatch implements ServerSideAction<Communication> {
	private static final long serialVersionUID = 5306067916641877356L;

	@Override
	public ServerSideActionResult<Communication> execute(Communication bean, WebContext webContext)
	throws Exception {
		
		bean.setRefreshBatches(Boolean.TRUE);
		
		String customerName = CORE.getUser().getCustomerName();
		File backupDir = new File(Util.getContentDirectory() + "batch_" + customerName + File.separator + bean.getSelectedBatchTimestampFolderName());
		if (backupDir.exists() && backupDir.isDirectory()) {
			FileUtil.delete(backupDir);
		}

		bean.setSelectedBatchTimestampFolderName(null); // deselect the deleted backup

		return new ServerSideActionResult<>(bean);
	}
	
}
