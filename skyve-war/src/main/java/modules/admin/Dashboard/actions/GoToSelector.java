package modules.admin.Dashboard.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.domain.DashboardWidget;

public class GoToSelector implements ServerSideAction<DashboardExtension> {
	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean,
			WebContext webContext) {

		bean.setFocusItem(DashboardWidget.newInstance());
		bean.setLoaded(null);
		bean.loadDashboard();

		return new ServerSideActionResult<>(bean);
	}
}
