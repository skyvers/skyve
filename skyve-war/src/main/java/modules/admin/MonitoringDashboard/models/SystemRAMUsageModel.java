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

public class SystemRAMUsageModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		ChartData cd = new ChartData();

		// Get monitoring start time and current time for all calculations
		long monitoringStartTime = Monitoring.getMonitoringStartTime();
		long currentTime = System.currentTimeMillis();

		cd.setTitle("System RAM Usage - Past 24 Hours");
		cd.setLabel("RAM Usage (%)");

		// Get system resource measurements
		ResourceMeasurements resourceMeasurements = Monitoring.getResourceMeasurements();

		// Data structures to hold our chart data
		List<String> hourLabels = new ArrayList<>();
		List<Number> ramValues = new ArrayList<>();

		// Get hourly RAM usage data (past 24 hours)
		Map<Integer, Float> hoursRAMData = resourceMeasurements.getHoursRAMPercentage();
		Color lineColor = Color.LIGHT_GRAY;

		// If no hourly data, fallback to minutes data
		if (hoursRAMData.isEmpty()) {
			Map<Integer, Float> minutesRAMData = resourceMeasurements.getMinutesRAMPercentage();

			if (!minutesRAMData.isEmpty()) {
				cd.setTitle("System RAM Usage - Past 60 Minutes");

				for (Map.Entry<Integer, Float> entry : minutesRAMData.entrySet()) {
					int minute = entry.getKey();
					float ramUsage = entry.getValue() * 100; // Convert decimal to percentage

					long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, minute, "minutes");
					LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());
					String minuteLabel = dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));

					hourLabels.add(minuteLabel);
					ramValues.add(ramUsage);

					// Color coding: green for low, yellow for medium, red for high RAM usage
					lineColor = getRAMUsageColor(ramUsage, 0.8f);
				}
			} else {
				// Fallback to seconds data if available
				Map<Integer, Float> secondsRAMData = resourceMeasurements.getSecondsRAMPercentage();

				if (!secondsRAMData.isEmpty()) {
					cd.setTitle("System RAM Usage - Past 60 Seconds");

					for (Map.Entry<Integer, Float> entry : secondsRAMData.entrySet()) {
						int second = entry.getKey();
						float ramUsage = entry.getValue() * 100; // Convert decimal to percentage

						long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, second, "seconds");
						LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis),
								ZoneId.systemDefault());
						String secondLabel = dateTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"));

						hourLabels.add(secondLabel);
						ramValues.add(ramUsage);
						lineColor = getRAMUsageColor(ramUsage, 0.8f);
					}
				}
			}
		} else {
			// Use hourly data with proper timestamps
			for (Map.Entry<Integer, Float> entry : hoursRAMData.entrySet()) {
				int hour = entry.getKey();
				float ramUsage = entry.getValue() * 100; // Convert decimal to percentage

				// Calculate actual timestamp for this hour index
				long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, hour, "hours");
				LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());
				String hourLabel = dateTime.format(DateTimeFormatter.ofPattern("MM/dd HH:00"));

				hourLabels.add(hourLabel);
				ramValues.add(ramUsage);

				// Color coding based on RAM usage
				lineColor = getRAMUsageColor(ramUsage, 0.8f);
			}
		}

		// If no data at all, show a placeholder
		if (hourLabels.isEmpty()) {
			hourLabels.add("No Data");
			ramValues.add(0);
			cd.setTitle("System RAM Usage - No Data Available");
		}

		// Set the chart data
		cd.setLabels(hourLabels);
		cd.setValues(ramValues);
		cd.setBackground(lineColor);
		cd.setBorder(lineColor);

		return cd;
	}

	private static long calculateTimestampForIndex(long startTime, long currentTime, int index, String timePeriod) {
		// Calculate how far back in time this index represents
		long timeIntervalMillis;
		long maxIntervals;

		switch (timePeriod) {
			case "seconds":
				timeIntervalMillis = 1000L; // 1 second
				maxIntervals = 60;
				break;
			case "minutes":
				timeIntervalMillis = 60 * 1000L; // 1 minute
				maxIntervals = 60;
				break;
			case "hours":
				timeIntervalMillis = 60 * 60 * 1000L; // 1 hour
				maxIntervals = 24;
				break;
			case "days":
				timeIntervalMillis = 24 * 60 * 60 * 1000L; // 1 day
				maxIntervals = 7;
				break;
			case "weeks":
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