package modules.admin.DataMaintenance.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class RefreshBackupList implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		
		//no code required - just force a rerender

		return new ServerSideActionResult<>(bean);
	}
}
