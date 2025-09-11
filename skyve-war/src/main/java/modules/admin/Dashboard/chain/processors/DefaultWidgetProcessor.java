package modules.admin.Dashboard.chain.processors;

import modules.admin.Dashboard.chain.AbstractDashboardProcessor;
import modules.admin.Dashboard.chain.DashboardProcessingContext;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.DashboardWidget;
import modules.admin.domain.DashboardWidget.WidgetType;

/**
 * Processor responsible for adding default widgets to a dashboard during loading.
 * This step only applies to dashboard loading, not activation.
 */
public class DefaultWidgetProcessor extends AbstractDashboardProcessor {

	@Override
	public boolean canHandle(DashboardProcessingContext context) {
		// Only handle load dashboard requests where widgets are empty
		return context.isLoadDashboard() &&
				context.getDashboard().getDashboardWidgets().isEmpty();
	}

	@Override
	protected DashboardProcessingContext doProcess(DashboardProcessingContext context) {
		addDefaultWidgets(context);
		return passToNext(context);
	}

	/**
	 * Adds default widgets to the dashboard if none are present.
	 */
	private static void addDefaultWidgets(DashboardProcessingContext context) {
		try {
			// Add default favourites widget
			DashboardWidgetExtension d1 = DashboardWidget.newInstance();
			d1.setWidgetType(WidgetType.favourites);
			context.getDashboard().addDashboardWidgetsElement(d1);

			// Add default pie chart widget
			DashboardWidgetExtension d2 = DashboardWidget.newInstance();
			d2.setWidgetType(WidgetType.mySystemUsageBreakdownPieChart);
			context.getDashboard().addDashboardWidgetsElement(d2);

			// Add default line chart widget
			DashboardWidgetExtension d3 = DashboardWidget.newInstance();
			d3.setWidgetType(WidgetType.mySystemUsageLineChart);
			context.getDashboard().addDashboardWidgetsElement(d3);

		} catch (Exception e) {
			context.setErrorMessage("Failed to add default widgets: " + e.getMessage());
		}
	}
}
