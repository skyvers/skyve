package modules.admin.Dashboard.dynamicModels;

import org.skyve.CORE;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.persistence.DocumentQuery;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.Dashboard;

public abstract class DynamicAbstractCustomChartModel extends ChartModel<DynamicBean> {

	@Override
	public ChartData getChartData() {
		DynamicBean bean = getBean();
		String moduleName = bean.getModuleMetaData()
				.getName();
		DocumentQuery qDashboard = CORE.getPersistence()
				.newDocumentQuery(Dashboard.MODULE_NAME, Dashboard.DOCUMENT_NAME);
		qDashboard.getFilter()
				.addEquals(Dashboard.moduleNamePropertyName, moduleName);
		DashboardExtension dashboard = qDashboard.beanResult();

		DashboardWidgetExtension widget = dashboard.findWidget(getModelNumber());

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
