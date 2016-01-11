package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class Restore implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 8521252561712649481L;

	@Override
	public ServerSideActionResult execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshContent(Boolean.TRUE);

		org.skyve.wildcat.backup.Truncate.truncate(bean.getSchemaName());
		org.skyve.wildcat.backup.Restore.restore(bean.getSelectedBackupTimestampFolderName());
		org.skyve.wildcat.backup.Reindex.reindex();

		return new ServerSideActionResult(bean);
	}
}
