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
 * Abstract base class for request-based monitoring chart models.
 * Provides common functionality for timestamp-based charting and data filtering for specific request types.
 */
public abstract class AbstractRequestChartModel extends ChartModel<MonitoringDashboard> {

	/**
	 * Get the chart title for this specific chart type and request.
	 * 
	 * @param requestKey The constructed request key
	 * @return The chart title
	 */
	protected abstract String getChartTitle(String requestKey);

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

		// Build the request key from user selections
		String requestKey = buildRequestKey(bean);

		ChartData cd = new ChartData();
		cd.setLabel(getChartLabel());

		// Data structures for time series
		List<String> timeLabels = new ArrayList<>();
		List<Number> values = new ArrayList<>();

		// Get measurements and use the user-selected period
		RequestMeasurements measurements = Monitoring.getRequestMeasurements(requestKey);

		Map<Integer, ? extends Number> chartData = null;
		Period period = bean.getRsPeriod() != null ? bean.getRsPeriod() : Period.pastDay;

		if (measurements != null) {
			// Use the user-selected period
			chartData = extractDataForTimePeriod(measurements, period);
		}

		// Build time series data with filtering for non-zero values
		buildTimeSeriesData(timeLabels, values, chartData, period);

		// Set chart properties
		cd.setTitle(getChartTitle(requestKey) + " (" + getTimePeriodLabel(period) + ")");
		cd.setLabels(timeLabels);
		cd.setValues(values);
		cd.setBackground(getChartColor());
		cd.setBorder(getChartColor());

		return cd;
	}

	/**
	 * Build a request key from the bean's request stats selections.
	 * Format: {type}{module}.{document}^{component}
	 */
	protected static String buildRequestKey(MonitoringDashboard bean) {
		StringBuilder keyBuilder = new StringBuilder();

		// Add request type (required)
		String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : "E";
		keyBuilder.append(requestType);

		// Add module name if specified
		String moduleName = bean.getRsModuleName();
		if (moduleName != null && !moduleName.trim().isEmpty()) {
			keyBuilder.append(moduleName.trim());
		}

		// Add document name if specified
		String documentName = bean.getRsDocumentName();
		if (documentName != null && !documentName.trim().isEmpty()) {
			keyBuilder.append('.').append(documentName.trim());
		}

		// Add component name if specified
		String componentName = bean.getRsComponentName();
		if (componentName != null && !componentName.trim().isEmpty()) {
			keyBuilder.append('^').append(componentName.trim());
		}

		return keyBuilder.toString();
	}

	/**
	 * Get a human-readable description of the request selection.
	 */
	protected static String getRequestDescription(MonitoringDashboard bean) {
		StringBuilder description = new StringBuilder();

		// Request type description
		String requestType = bean.getRsRequestType() != null ? bean.getRsRequestType().toCode() : "E";
		description.append(getRequestTypeDescription(requestType));

		// Add specifics if provided
		String moduleName = bean.getRsModuleName();
		String documentName = bean.getRsDocumentName();
		String componentName = bean.getRsComponentName();

		if (moduleName != null && !moduleName.trim().isEmpty()) {
			description.append(" - ").append(moduleName.trim());
			
			if (documentName != null && !documentName.trim().isEmpty()) {
				description.append(".").append(documentName.trim());
			}
			
			if (componentName != null && !componentName.trim().isEmpty()) {
				description.append(" (").append(componentName.trim()).append(")");
			}
		} else if (componentName != null && !componentName.trim().isEmpty()) {
			description.append(" - ").append(componentName.trim());
		}

		return description.toString();
	}

	/**
	 * Get human-readable description for request type codes.
	 */
	private static String getRequestTypeDescription(String requestType) {
		return switch (requestType) {
			case "C" -> "Create";
			case "E" -> "Edit";
			case "Q" -> "Query/List";
			case "P" -> "Map";
			case "H" -> "Chart";
			case "R" -> "Content";
			case "A" -> "AJAX";
			case "N" -> "Page";
			case "U" -> "SmartEdit";
			case "M" -> "Model";
			case "G" -> "Generate";
			case "L" -> "List";
			case "O" -> "Complete";
			case "S" -> "Search";
			case "Z" -> "Snap";
			case "T" -> "Tag";
			case "D" -> "Image";
			case "J" -> "Report";
			case "B" -> "Export";
			case "W" -> "Download";
			case "V" -> "Resource";
			default -> "Request";
		};
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