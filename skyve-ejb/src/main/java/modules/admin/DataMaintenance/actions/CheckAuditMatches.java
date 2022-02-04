package modules.admin.DataMaintenance.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class CheckAuditMatches implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		return new ServerSideActionResult<>(TruncateAuditLog.setResultCount(CORE.getPersistence(), bean));
	}
}
