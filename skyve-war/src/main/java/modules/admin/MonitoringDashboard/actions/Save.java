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
		return new ServerSideActionResult<>(CORE.getPersistence().save(bean));
	}
}
