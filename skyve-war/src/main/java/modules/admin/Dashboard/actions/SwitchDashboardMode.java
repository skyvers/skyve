package modules.admin.Dashboard.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Dashboard.DashboardExtension;

public class SwitchDashboardMode implements ServerSideAction<DashboardExtension> {
	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean,
			WebContext webContext) {
		if (Boolean.TRUE.equals(bean.getDesignMode())) {
			DashboardExtension savedBean = CORE.getPersistence()
					.save(bean);
			savedBean.setDesignMode(Boolean.FALSE);
			savedBean.setLoaded(Boolean.FALSE);
			savedBean.loadDashboard();
			return new ServerSideActionResult<>(savedBean);
		}

		// go to design mode
		bean.setDesignMode(Boolean.TRUE);
		bean.setLoaded(Boolean.FALSE);
		bean.loadDashboard();
		return new ServerSideActionResult<>(bean);
	}
}
