package modules.admin.MonitoringDashboard.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.web.WebContext;

import modules.admin.domain.MonitoringDashboard;

/**
 * Purges all measurements and starts again monitoring from the current time.
 */
public class Purge implements ServerSideAction<MonitoringDashboard> {
	@Override
	public ServerSideActionResult<MonitoringDashboard> execute(MonitoringDashboard bean, WebContext webContext) {
		Monitoring.purge();
		return new ServerSideActionResult<>(bean);
	}
}
