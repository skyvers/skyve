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

public class SystemRAMUsageModel extends ChartModel<MonitoringDashboard> {

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

		cd.setLabel("RAM Usage (%)");
		cd.setTitle("System RAM Usage - " + getPeriodLabel(period));

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
				long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, timeIndex, period);
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
			case pastMinute:
				return resourceMeasurements.getSecondsRAMPercentage();
			case pastHour:
				return resourceMeasurements.getMinutesRAMPercentage();
			case pastDay:
				return resourceMeasurements.getHoursRAMPercentage();
			case pastWeek:
				return resourceMeasurements.getDaysRAMPercentage();
			case pastYear:
				return resourceMeasurements.getWeeksRAMPercentage();
			default:
				return resourceMeasurements.getHoursRAMPercentage();
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