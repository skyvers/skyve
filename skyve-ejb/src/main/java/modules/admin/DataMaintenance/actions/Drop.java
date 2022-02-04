package modules.admin.DataMaintenance.actions;

import org.skyve.impl.backup.DDL;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class Drop implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		StringBuilder result = new StringBuilder(2048);
		for (String drop : DDL.drop(null, false)) {
			result.append(drop).append(';').append("<br/>");
		}
		
		bean.setDdlScript(result.toString());
		return new ServerSideActionResult<>(bean);
	}
}
