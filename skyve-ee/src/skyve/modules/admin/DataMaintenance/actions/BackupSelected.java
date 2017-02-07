package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class BackupSelected implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = 8136709192590507528L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshBackups(Boolean.FALSE);
		return new ServerSideActionResult<>(bean);
	}
}
