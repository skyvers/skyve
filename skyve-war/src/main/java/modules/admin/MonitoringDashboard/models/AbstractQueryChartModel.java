package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.Monitoring;
import org.skyve.util.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Abstract base class for query-based monitoring chart models.
 * Provides common functionality for timestamp-based charting and data filtering for query operations.
 */
public abstract class AbstractQueryChartModel extends ChartModel<MonitoringDashboard> {

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
	 * Get the request key for this chart type and query.
	 * 
	 * @param selectedQuery The selected query name
	 * @return The request key to use for monitoring data lookup
	 */
	protected abstract String getRequestKey(String selectedQuery);

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
		Period period = bean.getQueryStatsPeriod() != null ? bean.getQueryStatsPeriod() : Period.pastDay;

		if (measurements != null) {
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
	protected static String getSelectedQueryName(MonitoringDashboard bean) {
		if (bean.getQueryName() != null && !bean.getQueryName().trim().isEmpty()) {
			return bean.getQueryName();
		}
		return "All Queries";
	}

	/**
	 * Build time series data, only including time points with meaningful values.
	 */
	@SuppressWarnings("boxing")
	protected static void buildTimeSeriesData(List<String> timeLabels, List<Number> values,
			Map<Integer, ? extends Number> data, Period period) {

		// Get monitoring start time for proper time-based charting
		long monitoringStartTime = Monitoring.getMonitoringStartTime();
		long currentTime = System.currentTimeMillis();

		// Only include time points that have actual data (non-zero values)
		if (data != null) {
			for (Map.Entry<Integer, ? extends Number> entry : data.entrySet()) {
				Integer timeIndex = entry.getKey();
				Number value = entry.getValue();

				// Only add if there's a meaningful value (greater than 0)
				if (value != null && isSignificantValue(value)) {
					// Calculate actual timestamp for this index
					long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, timeIndex, period);

					// Add time label using actual timestamp
					timeLabels.add(formatTimestampLabel(timestampMillis, period));

					// Add the actual value
					values.add(value);
				}
			}
		}

		// If no data at all, add a placeholder
		if (timeLabels.isEmpty()) {
			timeLabels.add("No Data");
			values.add(0);
		}
	}

	/**
	 * Determine if a value is significant enough to include in the chart.
	 * Can be overridden by subclasses for different value types.
	 */
	protected static boolean isSignificantValue(Number value) {
		return value.doubleValue() > 0.0;
	}

	/**
	 * Calculate the actual timestamp for a given time index.
	 */
	protected static long calculateTimestampForIndex(long startTime, long currentTime, int index, Period period) {
		// Calculate how far back in time this index represents
		long timeIntervalMillis;
		long maxIntervals;

		switch (period) {
			case pastMinute:
				timeIntervalMillis = 1000L; // 1 second
				maxIntervals = 60;
				break;
			case pastHour:
				timeIntervalMillis = 60 * 1000L; // 1 minute
				maxIntervals = 60;
				break;
			case pastDay:
				timeIntervalMillis = 60 * 60 * 1000L; // 1 hour
				maxIntervals = 24;
				break;
			case pastWeek:
				timeIntervalMillis = 24 * 60 * 60 * 1000L; // 1 day
				maxIntervals = 7;
				break;
			case pastYear:
				timeIntervalMillis = 7 * 24 * 60 * 60 * 1000L; // 1 week
				maxIntervals = 52;
				break;
			default:
				timeIntervalMillis = 60 * 60 * 1000L; // Default to hours
				maxIntervals = 24;
		}

		// Calculate timestamp: current time minus the time offset for this index
		long timeOffset = (maxIntervals - 1 - index) * timeIntervalMillis;
		return Math.max(startTime, currentTime - timeOffset);
	}

	/**
	 * Format a timestamp for display on the chart axis.
	 */
	protected static String formatTimestampLabel(long timestampMillis, Period period) {
		LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());

		switch (period) {
			case pastMinute:
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"));
			case pastHour:
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));
			case pastDay:
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd HH:00"));
			case pastWeek:
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd"));
			case pastYear:
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd"));
			default:
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));
		}
	}

	/**
	 * Get the time period label with monitoring context.
	 */
	protected static String getTimePeriodLabel(Period period) {
		switch (period) {
			case pastMinute:
				return "Past 60 Seconds";
			case pastHour:
				return "Past 60 Minutes";
			case pastDay:
				return "Past 24 Hours";
			case pastWeek:
				return "Past 7 Days";
			case pastYear:
				return "Past 52 Weeks";
			default:
				return "Recent";
		}
	}
}