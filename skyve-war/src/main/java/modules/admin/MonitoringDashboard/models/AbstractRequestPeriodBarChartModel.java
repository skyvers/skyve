package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Abstract base class for request period average bar chart models.
 * Shows averages across different time periods (seconds, minutes, hours, days, weeks) as bars.
 */
public abstract class AbstractRequestPeriodBarChartModel extends AbstractMonitoringChartModel {

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
	protected abstract Color getChartColour();

	/**
	 * Extract the relevant data from RequestMeasurements for a specific time period.
	 * 
	 * @param measurements The request measurements data
	 * @param timePeriod The time period ("seconds", "minutes", "hours", "days", "weeks")
	 * @return Map of time index to measurement value, or null if no data
	 */
	protected abstract Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period);

	/**
	 * Determine if a value is significant enough to include in the chart.
	 */
	protected static boolean isSignificantValue(Number value) {
		return value != null && value.doubleValue() != 0.0;
	}

	/**
	 * Check if data is valid for a specific time period based on the last update time.
	 * If the last update was too long ago, certain periods should not be shown.
	 */
	protected static boolean isDataValidForPeriod(RequestMeasurements measurements, Period period) {
		if (measurements == null) {
			return false;
		}

		long lastUpdateTime = measurements.getTimeLastUpdate();
		if (lastUpdateTime <= 0) {
			return false; // No data recorded yet
		}

		LocalDateTime lastUpdate = LocalDateTime.ofInstant(Instant.ofEpochMilli(lastUpdateTime), ZoneId.systemDefault());
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

		// Get measurements for the specific request
		RequestMeasurements measurements = Monitoring.getRequestMeasurements(requestKey);

		if (measurements != null) {
			for (Period period : Period.values()) {
				// Check if data is valid for this time period
				if (! isDataValidForPeriod(measurements, period)) {
					continue; // Skip this period if data is too stale
				}

				// Get data for this time period
				Map<Integer, ? extends Number> data = extractDataForTimePeriod(measurements, period);

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

						periodLabels.add(period.toLocalisedDescription());
						averageValues.add(Double.valueOf(average));
					}
				}
			}
		}

		// If no data found, show placeholder
		if (periodLabels.isEmpty()) {
			periodLabels.add("No Recent Data");
			averageValues.add(Double.valueOf(0.0));
			cd.setTitle(getChartTitle(requestDescription) + " - No Recent Data Available");
		}

		// Set chart properties
		cd.setLabels(periodLabels);
		cd.setValues(averageValues);

		Color chartColour = getChartColour();
		cd.setBackground(chartColour);
		cd.setBorder(chartColour.darker());

		return cd;
	}
}