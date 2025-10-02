package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.Monitoring;
import org.skyve.util.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;

/**
 * Abstract base class for request period average bar chart models.
 * Shows averages across different time periods (seconds, minutes, hours, days, weeks) as bars.
 */
public abstract class AbstractRequestPeriodBarChartModel extends ChartModel<MonitoringDashboard> {

	/**
	 * Get the chart title for this specific chart type and request.
	 * 
	 * @param requestDescription The human-readable request description
	 * @return The chart title
	 */
	protected abstract String getChartTitle(String requestDescription);

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
	 * @param timePeriod The time period ("seconds", "minutes", "hours", "days", "weeks")
	 * @return Map of time index to measurement value, or null if no data
	 */
	protected abstract Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, String timePeriod);

	/**
	 * Determine if a value is significant enough to include in the chart.
	 * Can be overridden by subclasses for different value types.
	 */
	protected boolean isSignificantValue(Number value) {
		return value != null && value.doubleValue() != 0.0;
	}

	/**
	 * Check if data is valid for a specific time period based on the last update time.
	 * If the last update was too long ago, certain periods should not be shown.
	 */
	protected boolean isDataValidForPeriod(RequestMeasurements measurements, String timePeriod) {
		if (measurements == null) {
			return false;
		}

		long lastUpdateTime = measurements.getTimeLastUpdate();
		if (lastUpdateTime <= 0) {
			return false; // No data recorded yet
		}

		LocalDateTime lastUpdate = LocalDateTime.ofInstant(
			java.time.Instant.ofEpochMilli(lastUpdateTime), 
			java.time.ZoneId.systemDefault()
		);
		LocalDateTime now = LocalDateTime.now();

		// Calculate how long ago the last update was
		long minutesAgo = ChronoUnit.MINUTES.between(lastUpdate, now);
		long hoursAgo = ChronoUnit.HOURS.between(lastUpdate, now);
		long daysAgo = ChronoUnit.DAYS.between(lastUpdate, now);
		long weeksAgo = ChronoUnit.WEEKS.between(lastUpdate, now);

		// Determine validity based on time period and staleness
		return switch (timePeriod) {
			case "seconds" -> minutesAgo < 2; // Current minute: valid if updated within last 2 minutes
			case "minutes" -> hoursAgo < 2; // Current hour: valid if updated within last 2 hours
			case "hours" -> daysAgo < 2; // Current day: valid if updated within last 2 days
			case "days" -> weeksAgo < 2; // Current week: valid if updated within last 2 weeks
			case "weeks" -> weeksAgo < 8; // Current year: valid if updated within last ~2 months
			default -> false;
		};
	}

	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();

		// Build the request key from user selections
		String requestKey = buildRequestKey(bean);
		String requestDescription = getRequestDescription(bean);

		ChartData cd = new ChartData();
		cd.setLabel(getChartLabel());
		cd.setTitle(getChartTitle(requestDescription));

		// Data structures for bar chart
		List<String> periodLabels = new ArrayList<>();
		List<Number> averageValues = new ArrayList<>();
		List<Color> backgrounds = new ArrayList<>();
		List<Color> borders = new ArrayList<>();

		// Get measurements for the specific request
		RequestMeasurements measurements = Monitoring.getRequestMeasurements(requestKey);

		if (measurements != null) {
			// Time periods to analyze (from finest to coarsest)
			String[] timePeriods = {"seconds", "minutes", "hours", "days", "weeks"};
			String[] periodDisplayNames = {"Current minute", "Current hour", "Current day", "Current week", "Current year"};

			Color baseColor = getChartColor();
			
			for (int i = 0; i < timePeriods.length; i++) {
				String timePeriod = timePeriods[i];
				String displayName = periodDisplayNames[i];
				
				// Check if data is valid for this time period
				if (!isDataValidForPeriod(measurements, timePeriod)) {
					continue; // Skip this period if data is too stale
				}
				
				// Get data for this time period
				Map<Integer, ? extends Number> data = extractDataForTimePeriod(measurements, timePeriod);
				
				if (data != null && !data.isEmpty()) {
					// Calculate average of all non-zero values
					double total = 0.0;
					int count = 0;
					
					for (Number value : data.values()) {
						if (isSignificantValue(value)) {
							total += value.doubleValue();
							count++;
						}
					}
					
					if (count > 0) {
						double average = total / count;
						
						periodLabels.add(displayName);
						averageValues.add(average);
						
						// Create color variations for each bar
						float brightness = 1f; // Darker for longer periods
						Color barColor = new Color(
							(int)(baseColor.getRed() * brightness),
							(int)(baseColor.getGreen() * brightness),
							(int)(baseColor.getBlue() * brightness)
						);
						
						backgrounds.add(barColor);
						borders.add(barColor.darker());
					}
				}
			}
		}

		// If no data found, show placeholder
		if (periodLabels.isEmpty()) {
			periodLabels.add("No Recent Data");
			averageValues.add(0);
			backgrounds.add(Color.LIGHT_GRAY);
			borders.add(Color.GRAY);
			cd.setTitle(getChartTitle(requestDescription) + " - No Recent Data Available");
		}

		// Set chart properties
		// Create color variations for each bar
		Color baseColor = getChartColor();
		float brightness = 1f; // Darker for longer periods
		Color barColor = new Color(
			(int)(baseColor.getRed() * brightness),
			(int)(baseColor.getGreen() * brightness),
			(int)(baseColor.getBlue() * brightness)
		);
		cd.setLabels(periodLabels);
		cd.setValues(averageValues);
		cd.setBackground(barColor);
		cd.setBorder(barColor);

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
}