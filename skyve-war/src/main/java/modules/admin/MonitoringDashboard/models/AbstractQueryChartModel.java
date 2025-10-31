package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Abstract base class for query-based monitoring chart models.
 * Provides common functionality for timestamp-based charting and data filtering for query operations.
 */
public abstract class AbstractQueryChartModel extends AbstractMonitoringChartModel {

	/**
	 * Get the chart title for this specific chart type and query.
	 * 
	 * @param selectedQuery The name of the selected query
	 * @return The chart title
	 */
	protected abstract String getChartTitle(String selectedQuery);

	/**
	 * Get the chart label (Y-axis label) for this specific chart type.
	 * 
	 * @return The chart label
	 */
	protected abstract String getChartLabel();

	/**
	 * Get the chart color for this specific chart type.
	 * 
	 * @return The chart color
	 */
	protected abstract Color getChartColor();

	/**
	 * Extract the relevant data from RequestMeasurements for a specific time period.
	 * 
	 * @param measurements The request measurements data
	 * @param period The time period enum
	 * @return Map of time index to measurement value, or null if no data
	 */
	protected abstract Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period);

	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();

		// Get the selected query name (e.g., "admin.Users")
		String selectedQuery = getSelectedQueryName(bean);

		ChartData cd = new ChartData();
		cd.setLabel(getChartLabel());

		// Data structures for time series
		List<String> timeLabels = new ArrayList<>();
		List<Number> values = new ArrayList<>();

		// Get measurements and use the user-selected period
		String requestKey = getRequestKey(selectedQuery);
		RequestMeasurements measurements = Monitoring.getRequestMeasurements(requestKey);

		Map<Integer, ? extends Number> chartData = null;
		Period period = bean.getQueryStatsPeriod() != null ? bean.getQueryStatsPeriod() : Period.currentDay;

		if (measurements != null && isDataValidForCurrentPeriod(measurements, period)) {
			measurements.rollup();
			// Use the user-selected period
			chartData = extractDataForTimePeriod(measurements, period);
		}

		// Build time series data with filtering for non-zero values
		buildTimeSeriesData(timeLabels, values, chartData, period);

		// Set chart properties
		cd.setTitle(getChartTitle(selectedQuery) + " (" + getTimePeriodLabel(period) + ")");
		cd.setLabels(timeLabels);
		cd.setValues(values);
		cd.setBackground(getChartColor());
		cd.setBorder(getChartColor());

		return cd;
	}

	/**
	 * Get the selected query name from the bean, with fallback logic.
	 */
	private static String getSelectedQueryName(MonitoringDashboard bean) {
		if (bean.getQueryName() != null && !bean.getQueryName().trim().isEmpty()) {
			return bean.getQueryName();
		}
		return "All Queries";
	}
	
	private static String getRequestKey(String selectedQuery) {
		// Convert module.queryName to Mmodule^queryName format
		if (selectedQuery != null && selectedQuery.contains(".")) {
			return "M" + selectedQuery.replace(".", "^");
		}
		return "M" + selectedQuery;
	}
}