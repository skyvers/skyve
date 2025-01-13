package modules.admin.Dashboard.models;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;

public class FocusItemChartModel extends ChartModel<DashboardExtension> {

	@Override
	public ChartData getChartData() {

		DashboardExtension bean = getBean();

		DashboardWidgetExtension widget = bean.getFocusItem();

		// construct the custom chart
		if (widget != null) {
			return DashboardExtension.customChartModel(widget);
		}

		return null;
	}

}
