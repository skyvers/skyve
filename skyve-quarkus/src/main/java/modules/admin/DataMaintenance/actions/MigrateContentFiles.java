package modules.admin.DataMaintenance.actions;

//import org.skyve.impl.backup.ContentChecker;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

public class MigrateContentFiles implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext) throws Exception {
//		ContentChecker.migrateContentFiles();
		return new ServerSideActionResult<>(bean);
	}
}
