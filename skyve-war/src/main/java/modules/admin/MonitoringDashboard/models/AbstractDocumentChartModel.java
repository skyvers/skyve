package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
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
	 * @param period The time period enum
	 * @return Map of time index to measurement value, or null if no data
	 */
	protected abstract Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period);

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
		Period period = bean.getDocumentStatsPeriod() != null ? bean.getDocumentStatsPeriod() : Period.currentDay;

		if (measurements != null && isDataValidForCurrentPeriod(measurements, period)) {
			chartData = extractDataForTimePeriod(measurements, period);
		}

		// Build time series data with filtering for non-zero values
		buildTimeSeriesData(timeLabels, values, chartData, period);

		// Set chart properties
		cd.setTitle(getChartTitle(selectedDocument) + " (" + getTimePeriodLabel(period) + ")");
		cd.setLabels(timeLabels);
		cd.setValues(values);
		cd.setBackground(getChartColor());
		cd.setBorder(getChartColor());

		return cd;
	}

	/**
	 * Get the selected document name from the bean, with fallback logic.
	 */
	protected static String getSelectedDocumentName(MonitoringDashboard bean) {
		if (bean.getDocumentName() != null && !bean.getDocumentName().trim().isEmpty()) {
			return bean.getDocumentName();
		}
		return "All Documents";
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
				if (value != null && value.doubleValue() > 0.0) {
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
	 * Check if the measurement data is valid for the current period.
	 * Data is only valid if timeLastUpdate falls within the current period.
	 */
	protected static boolean isDataValidForCurrentPeriod(RequestMeasurements measurements, Period period) {
		long timeLastUpdate = measurements.getTimeLastUpdate();
		long currentTime = System.currentTimeMillis();
		
		LocalDateTime lastUpdateDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timeLastUpdate), ZoneId.systemDefault());
		LocalDateTime currentDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(currentTime), ZoneId.systemDefault());
		
		switch (period) {
			case currentMinute:
				// Must be within the same minute
				return lastUpdateDateTime.getYear() == currentDateTime.getYear() &&
					   lastUpdateDateTime.getDayOfYear() == currentDateTime.getDayOfYear() &&
					   lastUpdateDateTime.getHour() == currentDateTime.getHour() &&
					   lastUpdateDateTime.getMinute() == currentDateTime.getMinute();
			case currentHour:
				// Must be within the same hour
				return lastUpdateDateTime.getYear() == currentDateTime.getYear() &&
					   lastUpdateDateTime.getDayOfYear() == currentDateTime.getDayOfYear() &&
					   lastUpdateDateTime.getHour() == currentDateTime.getHour();
			case currentDay:
				// Must be within the same day
				return lastUpdateDateTime.getYear() == currentDateTime.getYear() &&
					   lastUpdateDateTime.getDayOfYear() == currentDateTime.getDayOfYear();
			case currentWeek:
				// Must be within the same week
				return lastUpdateDateTime.getYear() == currentDateTime.getYear() &&
					   lastUpdateDateTime.get(ChronoField.ALIGNED_WEEK_OF_YEAR) == currentDateTime.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
			case currentYear:
				// Must be within the same year
				return lastUpdateDateTime.getYear() == currentDateTime.getYear();
			default:
				return false;
		}
	}

	/**
	 * Calculate the actual timestamp for a given time index by replacing the appropriate time component.
	 */
	protected static long calculateTimestampForIndex(long startTime, long currentTime, int index, Period period) {
		LocalDateTime currentDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(currentTime), ZoneId.systemDefault());
		LocalDateTime resultDateTime;

		switch (period) {
			case currentMinute:
				// Replace seconds with index (0-59)
				resultDateTime = currentDateTime.withSecond(index).withNano(0);
				break;
			case currentHour:
				// Replace minutes with index (0-59), set seconds to 0
				resultDateTime = currentDateTime.withMinute(index).withSecond(0).withNano(0);
				break;
			case currentDay:
				// Replace hours with index (0-23), set minutes and seconds to 0
				resultDateTime = currentDateTime.withHour(index).withMinute(0).withSecond(0).withNano(0);
				break;
			case currentWeek:
				// Replace day of week with index (0-6), set time to start of day
				// Monday = 1, so index 0 should be Monday
				resultDateTime = currentDateTime.with(ChronoField.DAY_OF_WEEK, index + 1).withHour(0).withMinute(0).withSecond(0).withNano(0);
				break;
			case currentYear:
				// Replace week of year with index (0-51), set to Monday of that week
				resultDateTime = currentDateTime.with(ChronoField.ALIGNED_WEEK_OF_YEAR, index + 1).with(ChronoField.DAY_OF_WEEK, 1).withHour(0).withMinute(0).withSecond(0).withNano(0);
				break;
			default:
				// Default to current day behavior
				resultDateTime = currentDateTime.withHour(index).withMinute(0).withSecond(0).withNano(0);
		}

		return resultDateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
	}

	/**
	 * Format a timestamp for display on the chart axis.
	 */
	protected static String formatTimestampLabel(long timestampMillis, Period period) {
		LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());

		switch (period) {
			case currentMinute:
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"));
			case currentHour:
				return dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));
			case currentDay:
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd HH:00"));
			case currentWeek:
				return dateTime.format(DateTimeFormatter.ofPattern("MM/dd"));
			case currentYear:
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
			case currentMinute:
				return "Current Minute";
			case currentHour:
				return "Current Hour";
			case currentDay:
				return "Current Day";
			case currentWeek:
				return "Current Week";
			case currentYear:
				return "Current Year";
			default:
				return "Recent";
		}
	}
}