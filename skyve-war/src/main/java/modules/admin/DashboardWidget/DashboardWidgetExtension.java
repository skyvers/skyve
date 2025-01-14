package modules.admin.DashboardWidget;

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
			return this.getDashboardModule() != null && this.getModuleEntity() != null && this.getCategoryBinding() != null && this.getValueBinding() != null;
		}
		return true;
	}
}
