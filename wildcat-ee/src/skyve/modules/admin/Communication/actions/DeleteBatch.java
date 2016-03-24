package modules.admin.Communication.actions;

import java.io.File;

import modules.admin.domain.Communication;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;
import org.skyve.wildcat.util.UtilImpl;

public class DeleteBatch implements ServerSideAction<Communication> {
	private static final long serialVersionUID = 5306067916641877356L;

	@Override
	public ServerSideActionResult execute(Communication bean, WebContext webContext)
	throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		File backupDir = new File(UtilImpl.CONTENT_DIRECTORY + "batch_" + customerName + File.separator + bean.getSelectedBatchTimestampFolderName());
		if (backupDir.exists() && backupDir.isDirectory()) {
			delete(backupDir);
		}

		bean.setSelectedBatchTimestampFolderName(null); // deselect the deleted backup

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
