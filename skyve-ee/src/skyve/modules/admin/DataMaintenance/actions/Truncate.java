package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class Truncate implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -8003482363810304078L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshContent(Boolean.TRUE);

		org.skyve.impl.backup.Truncate.truncate(bean.getSchemaName(), true, true);
		return new ServerSideActionResult<>(bean);
	}
}
