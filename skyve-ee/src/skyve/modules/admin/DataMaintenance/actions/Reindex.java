package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class Reindex implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -5036413477264983775L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshContent(Boolean.TRUE);

		org.skyve.impl.backup.Reindex.reindex();
		return new ServerSideActionResult<>(bean);
	}
}
