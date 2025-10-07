package modules.admin.MonitoringDashboard.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.MonitoringDashboard;

/*
 * An action used to save the document during event handlers
 */
public class Save implements ServerSideAction<MonitoringDashboard> {
	@Override
	public ServerSideActionResult<MonitoringDashboard> execute(MonitoringDashboard bean,
			WebContext webContext) {
		// If request stats request type has changed then null out the module, document and component fields
		if (bean.originalValues().containsKey(MonitoringDashboard.rsRequestTypePropertyName)) {
			bean.setRsModuleName(null);
			bean.setRsDocumentName(null);
			bean.setRsComponentName(null);
		}
		return new ServerSideActionResult<>(CORE.getPersistence().save(bean));
	}
}
