package modules.admin.MonitoringDashboard.models;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;
import modules.admin.domain.MonitoringDashboard.RequestType;

/**
 * Abstract base class for all monitoring chart models.
 * Provides common functionality for timestamp calculation, data validation,
 * and time formatting across all monitoring charts.
 */
public abstract class AbstractMonitoringChartModel extends ChartModel<MonitoringDashboard> {
	/**
	 * Check if the request data is valid for the current period based on the last update time.
	 * If the last update was too long ago, the data should not be displayed.
	 */
	protected static boolean isDataValidForCurrentPeriod(RequestMeasurements measurements, Period period) {
		if (measurements == null) {
			return false;
		}

		long lastUpdateTime = measurements.getTimeLastUpdate();
		if (lastUpdateTime <= 0) {
			return false; // No data recorded yet
		}

		LocalDateTime lastUpdate = LocalDateTime.ofInstant(
				Instant.ofEpochMilli(lastUpdateTime),
				ZoneId.systemDefault());
		LocalDateTime now = LocalDateTime.now();

		// Calculate how long ago the last update was
		long minutesAgo = ChronoUnit.MINUTES.between(lastUpdate, now);
		long hoursAgo = ChronoUnit.HOURS.between(lastUpdate, now);
		long daysAgo = ChronoUnit.DAYS.between(lastUpdate, now);
		long weeksAgo = ChronoUnit.WEEKS.between(lastUpdate, now);

		// Determine validity based on time period and staleness
		return switch (period) {
			case currentMinute -> minutesAgo < 2; // Current minute: valid if updated within last 2 minutes
			case currentHour -> hoursAgo < 2; // Current hour: valid if updated within last 2 hours
			case currentDay -> daysAgo < 2; // Current day: valid if updated within last 2 days
			case currentWeek -> weeksAgo < 2; // Current week: valid if updated within last 2 weeks
			case currentYear -> weeksAgo < 8; // Current year: valid if updated within last ~2 months
		};
	}

	/**
	 * Get a human-readable label for the time period.
	 */
	protected static String getTimePeriodLabel(Period period) {
		return switch (period) {
			case currentMinute -> "Current Minute";
			case currentHour -> "Current Hour";
			case currentDay -> "Current Day";
			case currentWeek -> "Current Week";
			case currentYear -> "Current Year";
		};
	}

	/**
	 * Build time series data, only including time points with meaningful values.
	 */
	protected static void buildTimeSeriesData(List<String> timeLabels, List<Number> values,
			Map<Integer, ? extends Number> data, Period period) {

		// Get current time for proper time-based charting
		long currentTime = System.currentTimeMillis();

		// Only include time points that have actual data (non-zero values)
		if (data != null) {
			for (Map.Entry<Integer, ? extends Number> entry : data.entrySet()) {
				int timeIndex = entry.getKey().intValue();
				Number value = entry.getValue();

				// Only add if there's a meaningful value (greater than 0)
				if (value != null && value.doubleValue() > 0.0) {
					// Calculate actual timestamp for this index
					long timestampMillis = RequestListModel.calculateTimestampForIndex(currentTime, timeIndex, period);

					// Add time label using actual timestamp
					timeLabels.add(RequestListModel.formatTimestampLabel(timestampMillis, period));

					// Add the actual value
					values.add(value);
				}
			}
		}

		// If no data at all, add a placeholder
		if (timeLabels.isEmpty()) {
			timeLabels.add("No Data");
			values.add(Integer.valueOf(0));
		}
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
		description.append(RequestType.fromCode(requestType).toLocalisedDescription());

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
}