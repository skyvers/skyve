package modules.admin.Dashboard;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.metadata.module.Module;
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
		DashboardWidgetExtension focusItem = bean.getFocusItem();
		
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
				if (bean.getFocusItem() != null && !bean.getDashboardWidgets()
						.contains(bean.getFocusItem())) {
					// set defaults
					if (WidgetType.customChart.equals(bean.getFocusItem()
							.getWidgetType())) {
						focusItem.setTitle("New custom chart");
					}
				}
			}
			if (source.endsWith(DashboardWidget.aggregateFunctionPropertyName)
					|| source.endsWith(DashboardWidget.valueBindingPropertyName)) {
				// Check that aggregate function is not null while the valueBinding is a date/dateTime attribute. If so send a growl
				// message
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(focusItem
						.getDashboardModule());
				Document document = module.getDocument(customer, focusItem
						.getModuleEntity());
				Attribute attribute = document.getAttribute(focusItem
						.getValueBinding());
				boolean valueBindingNotNumber = true;
				if (attribute != null) {
					valueBindingNotNumber = AttributeType.date.equals(attribute.getAttributeType())
							|| AttributeType.dateTime.equals(attribute.getAttributeType());
				}
				boolean aggregateFunctionNull = focusItem
						.getAggregateFunction() == null;
				if (valueBindingNotNumber && aggregateFunctionNull) {
					webContext.growl(MessageSeverity.info,
							"Please ensure that the valueBinding is not of type Date/DateTime or is not \"Item\" when the aggregate function is null");
				}
			}
			if (source.endsWith(DashboardWidget.dashboardModulePropertyName)) {
				// If module changes, set entity, category and value to null
				focusItem.setModuleEntity(null);
				focusItem.setCategoryBinding(null);
				focusItem.setValueBinding(null);
			}
			if (source.endsWith(DashboardWidget.moduleEntityPropertyName)) {
				// If entity changes, category and value to null
				focusItem.setCategoryBinding(null);
				focusItem.setValueBinding(null);
			}
			bean.setLoaded(null);
			bean.loadDashboard();

		}
		super.preRerender(source, bean, webContext);
	}
}
