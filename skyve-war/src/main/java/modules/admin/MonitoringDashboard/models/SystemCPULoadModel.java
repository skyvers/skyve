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

public class SystemCPULoadModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();
		ChartData cd = new ChartData();

		// Get monitoring start time and current time for all calculations
		long monitoringStartTime = Monitoring.getMonitoringStartTime();
		long currentTime = System.currentTimeMillis();

		// Get user-selected period
		Period period = bean.getSystemResourcesPeriod() != null ? bean.getSystemResourcesPeriod() : Period.currentDay;

		cd.setLabel("CPU Load (cores)");
		cd.setTitle("System CPU Load - " + getPeriodLabel(period));

		// Get system resource measurements
		ResourceMeasurements resourceMeasurements = Monitoring.getResourceMeasurements();

		// Data structures to hold our chart data
		List<String> timeLabels = new ArrayList<>();
		List<Number> cpuValues = new ArrayList<>();

		// Get CPU data for the selected period
		Map<Integer, Float> cpuData = getCPUDataForPeriod(resourceMeasurements, period);
		Color lineColor = Color.LIGHT_GRAY;

		// Build chart data using the selected period
		if (!cpuData.isEmpty()) {
			for (Map.Entry<Integer, Float> entry : cpuData.entrySet()) {
				int timeIndex = entry.getKey();
				float cpuLoad = entry.getValue();

				// Calculate actual timestamp for this time index
				long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, timeIndex, period);
				LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());
				String timeLabel = formatTimestampLabel(dateTime, period);

				timeLabels.add(timeLabel);
				cpuValues.add(cpuLoad);

				// Color coding based on CPU load
				lineColor = getCPULoadColor(cpuLoad, 0.8f);
			}
		}

		// If no data at all, show a placeholder
		if (timeLabels.isEmpty()) {
			timeLabels.add("No Data");
			cpuValues.add(0);
			cd.setTitle("System CPU Load - No Data Available");
		}

		// Set the chart data
		cd.setLabels(timeLabels);
		cd.setValues(cpuValues);
		cd.setBackground(lineColor);
		cd.setBorder(lineColor);

		return cd;
	}

	/**
	 * Get CPU data for the specified period.
	 */
	private static Map<Integer, Float> getCPUDataForPeriod(ResourceMeasurements resourceMeasurements, Period period) {
		switch (period) {
			case currentMinute:
				return resourceMeasurements.getSecondsCPUCoresUsage();
			case currentHour:
				return resourceMeasurements.getMinutesCPUCoresUsage();
			case currentDay:
				return resourceMeasurements.getHoursCPUCoresUsage();
			case currentWeek:
				return resourceMeasurements.getDaysCPUCoresUsage();
			case currentYear:
				return resourceMeasurements.getWeeksCPUCoresUsage();
			default:
				return resourceMeasurements.getHoursCPUCoresUsage();
		}
	}

	/**
	 * Get period label for chart title.
	 */
	private static String getPeriodLabel(Period period) {
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
				return "Current Day";
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
	private static long calculateTimestampForIndex(long startTime, long currentTime, int index, Period period) {
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

		return timestamp.toEpochSecond(java.time.ZoneOffset.UTC) * 1000; // Convert to milliseconds
	}

	/**
	 * Get color based on CPU load level
	 * 
	 * @param cpuLoad CPU load value (in cores)
	 * @param saturation Color saturation (0.0 to 1.0)
	 * @return Color representing the CPU load level
	 */
	private static Color getCPULoadColor(float cpuLoad, float saturation) {
		float hue;

		if (cpuLoad < 0.5f) {
			// Low load: Green
			hue = 0.33f; // Green
		} else if (cpuLoad < 1.0f) {
			// Medium load: Yellow
			hue = 0.17f; // Yellow
		} else if (cpuLoad < 2.0f) {
			// High load: Orange
			hue = 0.08f; // Orange
		} else {
			// Very high load: Red
			hue = 0.0f; // Red
		}

		return Color.getHSBColor(hue, saturation, 0.9f);
	}
}