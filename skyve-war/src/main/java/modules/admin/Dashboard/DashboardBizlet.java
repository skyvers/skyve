package modules.admin.Dashboard;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardWidget;
import modules.admin.domain.DashboardWidget.WidgetType;

public class DashboardBizlet extends SingletonCachedBizlet<DashboardExtension> {
	@Override
	public DashboardExtension newInstance(DashboardExtension bean) throws Exception {
		// TODO Auto-generated method stub
		return super.newInstance(bean);
	}

	@Override
	public DashboardExtension preExecute(ImplicitActionName actionName, DashboardExtension bean, Bean parentBean, WebContext webContext)
			throws Exception {
		if (ImplicitActionName.New.equals(actionName)) {
			bean.setUser(ModulesUtil.currentAdminUser());
			/*AssuranceManagementStaffExtension staff = SamsUtil.staffForUser(CORE.getUser().getId());
			if(staff!=null) {
				SamsUtil.setCurrentStaffId(staff);
				SamsUtil.setProgramContextId(staff.getProgramContext());
			}*/
			
			bean.loadDashboard();
			
			
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	@Override
	public void preRerender(String source, DashboardExtension bean, WebContext webContext) throws Exception {

		// if an item is selected in the grid - show it as the focus item for further design
		if (Dashboard.dashboardWidgetsPropertyName.equals(source)) {
			if (bean.getSelectedExistingItemId() != null) {
				bean.setFocusItem(bean.getDashboardWidgetsElementById(bean.getSelectedExistingItemId()));
				bean.setLoaded(null);
				bean.loadDashboard();
			} else {
				bean.setFocusItem(DashboardWidget.newInstance());
			}
		}
		if (source.startsWith(Dashboard.focusItemPropertyName)) {
			if (source.endsWith(DashboardWidget.widgetTypePropertyName)) {
				if (bean.getFocusItem() != null && !bean.getDashboardWidgets().contains(bean.getFocusItem())) {
					// set defaults
					if (WidgetType.customChart.equals(bean.getFocusItem().getWidgetType())) {
						DashboardWidgetExtension focusItem = bean.getFocusItem();
						focusItem.setTitle("New custom chart");
					}
				}
			}
			bean.setLoaded(null);
			bean.loadDashboard();

		}
		super.preRerender(source, bean, webContext);
	}
}
