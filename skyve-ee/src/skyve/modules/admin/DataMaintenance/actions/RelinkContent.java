package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;
import org.skyve.impl.backup.ContentChecker;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class RelinkContent implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -3751956632018540428L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext) throws Exception {

		new ContentChecker().relinkContent();
		
		return new ServerSideActionResult<>(bean);
	}
}
