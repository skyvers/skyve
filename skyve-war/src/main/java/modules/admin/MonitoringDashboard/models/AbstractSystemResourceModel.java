package modules.admin.MonitoringDashboard.models;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Abstract base class for system resource monitoring models.
 * Provides common functionality for timestamp calculation, formatting, and chart data setup.
 */
public abstract class AbstractSystemResourceModel extends ChartModel<MonitoringDashboard> {

	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();
		ChartData cd = new ChartData();

		long currentTime = System.currentTimeMillis();

		// Get user-selected period
		Period period = bean.getSystemResourcesPeriod() != null ? bean.getSystemResourcesPeriod() : Period.currentDay;

		cd.setLabel(getChartLabel());
		cd.setTitle(getChartTitle(period));

		// Get system resource measurements
		ResourceMeasurements resourceMeasurements = Monitoring.getResourceMeasurements();

		// Data structures to hold our chart data
		List<String> timeLabels = new ArrayList<>();
		List<Number> values = new ArrayList<>();

		// Get resource data for the selected period
		Map<Integer, ? extends Number> resourceData = getResourceDataForPeriod(resourceMeasurements, period);

		// Build chart data using the selected period
		if (!resourceData.isEmpty()) {
			for (Map.Entry<Integer, ? extends Number> entry : resourceData.entrySet()) {
				int timeIndex = entry.getKey().intValue();
				Number value = entry.getValue();

				// Calculate actual timestamp for this time index
				long timestampMillis = RequestListModel.calculateTimestampForIndex(currentTime, timeIndex, period);
				String timeLabel = RequestListModel.formatTimestampLabel(timestampMillis, period);

				timeLabels.add(timeLabel);
				values.add(value);
			}
		}

		// If no data at all, show a placeholder
		if (timeLabels.isEmpty()) {
			timeLabels.add("No Data");
			values.add(Integer.valueOf(0));
			cd.setTitle(getChartTitle(period) + " - No Data Available");
		}

		// Set the chart data
		cd.setLabels(timeLabels);
		cd.setValues(values);

		// Set colors based on the resource type and values
		setChartColors(cd, values);

		return cd;
	}

	/**
	 * Get the chart label for this resource type.
	 */
	protected abstract String getChartLabel();

	/**
	 * Get the chart title for this resource type and period.
	 */
	protected abstract String getChartTitle(Period period);

	/**
	 * Get resource data for the specified period.
	 */
	protected abstract Map<Integer, ? extends Number> getResourceDataForPeriod(ResourceMeasurements resourceMeasurements,
			Period period);

	/**
	 * Set chart colors based on the resource values.
	 */
	protected abstract void setChartColors(ChartData cd, List<Number> values);
}