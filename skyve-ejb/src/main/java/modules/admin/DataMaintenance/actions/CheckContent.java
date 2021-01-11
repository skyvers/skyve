package modules.admin.DataMaintenance.actions;

import modules.admin.domain.DataMaintenance;
import org.skyve.impl.backup.ContentChecker;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class CheckContent implements ServerSideAction<DataMaintenance> {
	private static final long serialVersionUID = -2943997026132660437L;

	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext) throws Exception {

		new ContentChecker().checkContent();
		
		return new ServerSideActionResult<>(bean);
	}
}
