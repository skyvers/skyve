package modules.admin.Dashboard.models;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;

public class CustomChartModel9 extends ChartModel<DashboardExtension> {

	public static final int THIS_MODEL_NUMBER = 9;

	@Override
	public ChartData getChartData() {

		DashboardExtension bean = getBean();

		DashboardWidgetExtension widget = bean.findWidget(THIS_MODEL_NUMBER);

		// construct the custom chart
		if (widget != null) {
			return DashboardExtension.customChartModel(widget);
		}

		return null;
	}

}
