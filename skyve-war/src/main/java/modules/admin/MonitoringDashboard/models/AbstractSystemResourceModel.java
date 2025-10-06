package modules.admin.MonitoringDashboard.models;

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
import org.skyve.util.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Abstract base class for system resource monitoring models.
 * Provides common functionality for timestamp calculation, formatting, and chart data setup.
 */
public abstract class AbstractSystemResourceModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
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
				int timeIndex = entry.getKey();
				Number value = entry.getValue();

				// Calculate actual timestamp for this time index
				long timestampMillis = calculateTimestampForIndex(currentTime, timeIndex, period);
				LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());
				String timeLabel = formatTimestampLabel(dateTime, period);

				timeLabels.add(timeLabel);
				values.add(value);
			}
		}

		// If no data at all, show a placeholder
		if (timeLabels.isEmpty()) {
			timeLabels.add("No Data");
			values.add(0);
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

	/**
	 * Format timestamp label based on period.
	 */
	protected static String formatTimestampLabel(LocalDateTime dateTime, Period period) {
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
	 * Calculate timestamp by replacing the appropriate time component with the given index.
	 * This ensures timestamps align with the actual time structure rather than using subtraction.
	 */
	protected static long calculateTimestampForIndex(long currentTime, int index, Period period) {
		LocalDateTime now = LocalDateTime.ofInstant(Instant.ofEpochMilli(currentTime), ZoneId.systemDefault());
		LocalDateTime timestamp;

		switch (period) {
			case currentMinute:
				// Replace seconds with index (0-59)
				timestamp = now.with(ChronoField.SECOND_OF_MINUTE, index);
				break;
			case currentHour:
				// Replace minutes with index (0-59)
				timestamp = now.with(ChronoField.MINUTE_OF_HOUR, index);
				break;
			case currentDay:
				// Replace hours with index (0-23)
				timestamp = now.with(ChronoField.HOUR_OF_DAY, index);
				break;
			case currentWeek:
				// Replace day of week with index (0=Monday, 6=Sunday)
				// ChronoField.DAY_OF_WEEK uses 1=Monday, 7=Sunday, so add 1
				timestamp = now.with(ChronoField.DAY_OF_WEEK, index + 1);
				break;
			case currentYear:
				// Replace week of year with index (0-51, so add 1 for 1-52)
				timestamp = now.with(ChronoField.ALIGNED_WEEK_OF_YEAR, index + 1);
				break;
			default:
				timestamp = now;
				break;
		}

		return timestamp.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
	}
}