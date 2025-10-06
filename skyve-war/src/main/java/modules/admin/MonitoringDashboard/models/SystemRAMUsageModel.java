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
import org.skyve.util.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

public class SystemRAMUsageModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();
		ChartData cd = new ChartData();

		long currentTime = System.currentTimeMillis();

		// Get user-selected period
		Period period = bean.getSystemResourcesPeriod() != null ? bean.getSystemResourcesPeriod() : Period.currentDay;

		cd.setLabel("RAM Usage (%)");
		cd.setTitle("System RAM Usage - " + period.toLocalisedDescription());

		// Get system resource measurements
		ResourceMeasurements resourceMeasurements = Monitoring.getResourceMeasurements();

		// Data structures to hold our chart data
		List<String> timeLabels = new ArrayList<>();
		List<Number> ramValues = new ArrayList<>();

		// Get RAM data for the selected period
		Map<Integer, Float> ramData = getRAMDataForPeriod(resourceMeasurements, period);
		Color lineColor = Color.LIGHT_GRAY;

		// Build chart data using the selected period
		if (!ramData.isEmpty()) {
			for (Map.Entry<Integer, Float> entry : ramData.entrySet()) {
				int timeIndex = entry.getKey();
				float ramUsage = entry.getValue();

				// Calculate actual timestamp for this time index
				long timestampMillis = calculateTimestampForIndex(currentTime, timeIndex, period);
				LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());
				String timeLabel = formatTimestampLabel(dateTime, period);

				timeLabels.add(timeLabel);
				ramValues.add(ramUsage);

				// Color coding based on RAM usage
				lineColor = getRAMUsageColor(ramUsage, 0.8f);
			}
		}

		// If no data at all, show a placeholder
		if (timeLabels.isEmpty()) {
			timeLabels.add("No Data");
			ramValues.add(0);
			cd.setTitle("System RAM Usage - No Data Available");
		}

		// Set the chart data
		cd.setLabels(timeLabels);
		cd.setValues(ramValues);
		cd.setBackground(lineColor);
		cd.setBorder(lineColor);

		return cd;
	}

	/**
	 * Get RAM data for the specified period.
	 */
	private static Map<Integer, Float> getRAMDataForPeriod(ResourceMeasurements resourceMeasurements, Period period) {
		switch (period) {
			case currentMinute:
				return resourceMeasurements.getSecondsRAMPercentage();
			case currentHour:
				return resourceMeasurements.getMinutesRAMPercentage();
			case currentDay:
				return resourceMeasurements.getHoursRAMPercentage();
			case currentWeek:
				return resourceMeasurements.getDaysRAMPercentage();
			case currentYear:
				return resourceMeasurements.getWeeksRAMPercentage();
			default:
				return resourceMeasurements.getHoursRAMPercentage();
		}
	}

	/**
	 * Format timestamp label based on period.
	 */
	private static String formatTimestampLabel(LocalDateTime dateTime, Period period) {
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
	private static long calculateTimestampForIndex(long currentTime, int index, Period period) {
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

		return timestamp.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(); // Convert to milliseconds
	}

	/**
	 * Get color based on RAM usage percentage
	 * 
	 * @param ramUsage RAM usage percentage (0-100)
	 * @param saturation Color saturation (0.0 to 1.0)
	 * @return Color representing the RAM usage level
	 */
	private static Color getRAMUsageColor(float ramUsage, float saturation) {
		float hue;

		if (ramUsage < 50.0f) {
			// Low usage: Green
			hue = 0.33f; // Green
		} else if (ramUsage < 70.0f) {
			// Medium usage: Yellow
			hue = 0.17f; // Yellow
		} else if (ramUsage < 85.0f) {
			// High usage: Orange
			hue = 0.08f; // Orange
		} else {
			// Critical usage: Red
			hue = 0.0f; // Red
		}

		return Color.getHSBColor(hue, saturation, 0.9f);
	}
}