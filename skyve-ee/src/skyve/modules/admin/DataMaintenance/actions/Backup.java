package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class Backup implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -2943997026132660437L;

	@Override
	public ServerSideActionResult execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshBackups(Boolean.TRUE);
		
		org.skyve.impl.backup.Backup.backup();
		return new ServerSideActionResult(bean);
	}
}
