package modules.admin.DashboardWidget;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

import modules.admin.domain.DashboardWidget;

public class DashboardWidgetExtension extends DashboardWidget {

	private static final long serialVersionUID = 4050004952643908669L;

	/**
	 * Whether to show all the custom chart optons
	 * - only if custom chart type is selected
	 * 
	 * @return
	 */
	public boolean showCustomChartOptions() {
		return WidgetType.customChart.equals(this.getWidgetType());
	}

	/**
	 * Whether the item has sufficient information to preview
	 * 
	 * @return
	 */
	public boolean sufficientInformationToPreview() {
		if (WidgetType.customChart.equals(this.getWidgetType())) {
			if (this.getDashboardModule() != null && this.getModuleEntity() != null && this.getCategoryBinding() != null
					&& this.getValueBinding() != null) {
				// Check that aggregate function is not null while the valueBinding is a date/dateTime attribute
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(getDashboardModule());
				Document document = module.getDocument(customer, getModuleEntity());
				Attribute attribute = document.getAttribute(getValueBinding());
				boolean valueBindingNotNumber = true;
				if(attribute != null) {
					valueBindingNotNumber = AttributeType.date.equals(attribute.getAttributeType())
							|| AttributeType.dateTime.equals(attribute.getAttributeType());
				}
				if (valueBindingNotNumber && getAggregateFunction() == null) {
					return false;
				}
				return true;
			}
			return false;
		}
		return true;
	}
}
