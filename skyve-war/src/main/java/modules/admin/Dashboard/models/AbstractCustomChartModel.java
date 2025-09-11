package modules.admin.Dashboard.models;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;

public abstract class AbstractCustomChartModel extends ChartModel<DashboardExtension> {

	@Override
	public ChartData getChartData() {
		DashboardExtension bean = getBean();

		DashboardWidgetExtension widget = bean.findWidget(getModelNumber());

		// construct the custom chart
		if (widget != null) {
			return DashboardExtension.customChartModel(widget);
		}

		return null;
	}

	/**
	 * The model number of this dashboard, to be implemented by a subclass.
	 * 
	 * @return The model number, e.g. 1-9
	 */
	public abstract int getModelNumber();

}
