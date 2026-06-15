package modules.admin.MonitoringDashboard.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.MonitoringDashboard;

/**
 * An action used to save the document during event handlers
 */
public class Save implements ServerSideAction<MonitoringDashboard> {
	/**
	 * Saves the dashboard and clears dependent request-stat filters when the request type changes.
	 * @param bean the dashboard being saved
	 * @param webContext the current web context
	 * @return the action result containing the saved dashboard
	 */
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
