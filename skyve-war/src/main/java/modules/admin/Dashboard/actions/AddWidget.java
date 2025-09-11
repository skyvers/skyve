package modules.admin.Dashboard.actions;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.domain.DashboardWidget;

public class AddWidget implements ServerSideAction<DashboardExtension> {
	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean,
			WebContext webContext) {
		
		if (bean.getFocusItem() != null) {
			if (!bean.getFocusItem().sufficientInformationToPreview()) {
				throw new ValidationException(new Message("You have not set enough items yet to be able to add this widget"));
			}
			bean.addDashboardWidgetsElement(bean.getFocusItem());
			bean.setFocusItem(DashboardWidget.newInstance());
			bean.setLoaded(null);
			bean.loadDashboard();
		}
		
		return new ServerSideActionResult<>(bean);
	}
}
