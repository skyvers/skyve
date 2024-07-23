package modules.admin.DataMaintenance.actions;

import org.skyve.impl.backup.DDL;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class Sync implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		StringBuilder result = new StringBuilder(2048);
		for (String update : DDL.sync(false)) {
			result.append(update).append(';').append("<br/>");
		}
		
		bean.setDdlScript(result.toString());
		return new ServerSideActionResult<>(bean);
	}
}
