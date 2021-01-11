package modules.admin.DataMaintenance.actions;

//import org.skyve.impl.backup.ContentChecker;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class MigrateContent implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -1081095413688690954L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext) throws Exception {
//		new ContentChecker().migrateContent();
		return new ServerSideActionResult<>(bean);
	}
}
