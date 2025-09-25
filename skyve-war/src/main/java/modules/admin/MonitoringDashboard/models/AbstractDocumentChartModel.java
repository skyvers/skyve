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

/**
 * Abstract base class for document-based monitoring chart models.
 * Provides common functionality for timestamp-based charting and data filtering.
 */
public abstract class AbstractDocumentChartModel extends ChartModel<MonitoringDashboard> {

	/**
	 * Get the chart title for this specific chart type and document.
	 * 
	 * @param selectedDocument The name of the selected document
	 * @return The chart title
	 */
	protected abstract String getChartTitle(String selectedDocument);

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
	 * Get the request key for this chart type and document.
	 * 
	 * @param selectedDocument The selected document name
	 * @return The request key to use for monitoring data lookup
	 */
	protected abstract String getRequestKey(String selectedDocument);

	/**
	 * Extract the relevant data from RequestMeasurements for a specific time period.
	 * 
	 * @param measurements The request measurements data
	 * @param timePeriod The time period ("hours", "minutes", "seconds")
	 * @return Map of time index to measurement value, or null if no data
	 */
	protected abstract Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, String timePeriod);

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();

		// Get the selected document name (e.g., "admin.Audit")
		String selectedDocument = getSelectedDocumentName(bean);

		ChartData cd = new ChartData();
		cd.setLabel(getChartLabel());

		// Data structures for time series
		List<String> timeLabels = new ArrayList<>();
		List<Number> values = new ArrayList<>();

		// Get measurements and determine best time period with data
		String requestKey = getRequestKey(selectedDocument);
		RequestMeasurements measurements = Monitoring.getRequestMeasurements(requestKey);

		Map<Integer, ? extends Number> chartData = null;
		String timePeriod = "hours";

		if (measurements != null) {
			// Try to find data in order of preference: hours -> minutes -> seconds
			chartData = extractDataForTimePeriod(measurements, "hours");
			if (chartData == null || chartData.isEmpty()) {
				chartData = extractDataForTimePeriod(measurements, "minutes");
				timePeriod = "minutes";
				if (chartData == null || chartData.isEmpty()) {
					chartData = extractDataForTimePeriod(measurements, "seconds");
					timePeriod = "seconds";
				}
			}
		}

		// Build time series data with filtering for non-zero values
		buildTimeSeriesData(timeLabels, values, chartData, timePeriod);

		// Set chart properties
		cd.setTitle(getChartTitle(selectedDocument) + " (" + getTimePeriodLabel(timePeriod) + ")");
		cd.setLabels(timeLabels);
		cd.setValues(values);
		cd.setBackground(getChartColor());
		cd.setBorder(getChartColor());

		return cd;
	}

	/**
	 * Get the selected document name from the bean, with fallback logic.
	 */
	protected String getSelectedDocumentName(MonitoringDashboard bean) {
		if (bean.getDocumentName() != null && !bean.getDocumentName().trim().isEmpty()) {
			return bean.getDocumentName();
		}
		return "All Documents";
	}

	/**
	 * Build time series data, only including time points with meaningful values.
	 */
	protected void buildTimeSeriesData(List<String> timeLabels, List<Number> values,
			Map<Integer, ? extends Number> data, String timePeriod) {

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
					long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, timeIndex, timePeriod);

					// Add time label using actual timestamp
					timeLabels.add(formatTimestampLabel(timestampMillis, timePeriod));

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
	protected boolean isSignificantValue(Number value) {
		return value.doubleValue() > 0.0;
	}

	/**
	 * Calculate the actual timestamp for a given time index.
	 */
	protected long calculateTimestampForIndex(long startTime, long currentTime, int index, String timePeriod) {
		// Calculate how far back in time this index represents
		long timeIntervalMillis;
		long maxIntervals;

		switch (timePeriod) {
			case "seconds":
				timeIntervalMillis = 1000L; // 1 second
				maxIntervals = 60;
				break;
			case "minutes":
				timeIntervalMillis = 60 * 1000L; // 1 minute
				maxIntervals = 60;
				break;
			case "hours":
				timeIntervalMillis = 60 * 60 * 1000L; // 1 hour
				maxIntervals = 24;
				break;
			case "days":
				timeIntervalMillis = 24 * 60 * 60 * 1000L; // 1 day
				maxIntervals = 7;
				break;
			case "weeks":
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
	protected String formatTimestampLabel(long timestampMillis, String timePeriod) {
		LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());

		switch (timePeriod) {
			case "seconds":
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"));
			case "minutes":
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));
			case "hours":
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd HH:00"));
			case "days":
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd"));
			case "weeks":
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd"));
			default:
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));
		}
	}

	/**
	 * Get the time period label with monitoring context.
	 */
	protected String getTimePeriodLabel(String timePeriod) {

		switch (timePeriod) {
			case "seconds":
				return "Past 60 Seconds";
			case "minutes":
				return "Past 60 Minutes";
			case "hours":
				return "Past 24 Hours";
			case "days":
				return "Past 7 Days";
			case "weeks":
				return "Past 52 Weeks";
			default:
				return "Recent";
		}
	}
}