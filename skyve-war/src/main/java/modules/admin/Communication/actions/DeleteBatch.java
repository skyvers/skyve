package modules.admin.Communication.actions;

import java.io.File;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.FileUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

/**
 * Deletes generated output files for a selected communication batch.
 */
public class DeleteBatch implements ServerSideAction<Communication> {
	/**
	 * Performs the execute operation.
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<Communication> execute(Communication bean, WebContext webContext)
	throws Exception {
		
		bean.setRefreshBatches(Boolean.TRUE);
		
		File backupDir = new File(bean.getBasePath() + File.separator + bean.getSelectedBatchTimestampFolderName());
		if (backupDir.exists() && backupDir.isDirectory()) {
			FileUtil.delete(backupDir);
		}

		// deselect the deleted backup
		bean.setSelectedBatchTimestampFolderName(null);

		return new ServerSideActionResult<>(bean);
	}
}
