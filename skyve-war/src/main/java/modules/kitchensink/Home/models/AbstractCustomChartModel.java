package modules.kitchensink.Home.models;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.kitchensink.domain.Home;

public abstract class AbstractCustomChartModel extends ChartModel<Home> {

	@Override
	public ChartData getChartData() {

		Home bean = getBean();

		DashboardWidgetExtension widget = bean.getDashboard().findWidget(getModelNumber());

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
