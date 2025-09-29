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
		Period period = bean.getSystemResourcesPeriod() != null ? bean.getSystemResourcesPeriod() : Period.pastDay;

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
			case pastMinute:
				return resourceMeasurements.getSecondsCPUCoresUsage();
			case pastHour:
				return resourceMeasurements.getMinutesCPUCoresUsage();
			case pastDay:
				return resourceMeasurements.getHoursCPUCoresUsage();
			case pastWeek:
				return resourceMeasurements.getDaysCPUCoresUsage();
			case pastYear:
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
				return "Past 24 Hours";
		}
	}

	/**
	 * Format timestamp label based on period.
	 */
	private static String formatTimestampLabel(LocalDateTime dateTime, Period period) {
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

	private static long calculateTimestampForIndex(long startTime, long currentTime, int index, Period period) {
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