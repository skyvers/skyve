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

public class SystemCPULoadModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		ChartData cd = new ChartData();

		// Get monitoring start time and current time for all calculations
		long monitoringStartTime = Monitoring.getMonitoringStartTime();
		long currentTime = System.currentTimeMillis();

		cd.setTitle("System CPU Load - Past 24 Hours");
		cd.setLabel("CPU Load (cores)");

		// Get system resource measurements
		ResourceMeasurements resourceMeasurements = Monitoring.getResourceMeasurements();

		// Data structures to hold our chart data
		List<String> hourLabels = new ArrayList<>();
		List<Number> cpuValues = new ArrayList<>();

		// Get hourly CPU usage data (past 24 hours)
		Map<Integer, Float> hoursCPUData = resourceMeasurements.getHoursCPUCoresUsage();
		Color lineColor = Color.LIGHT_GRAY;

		// If no hourly data, fallback to minutes data
		if (hoursCPUData.isEmpty()) {

			Map<Integer, Float> minutesCPUData = resourceMeasurements.getMinutesCPUCoresUsage();

			if (!minutesCPUData.isEmpty()) {
				cd.setTitle("System CPU Load - Past 60 Minutes");

				for (Map.Entry<Integer, Float> entry : minutesCPUData.entrySet()) {
					int minute = entry.getKey();
					float cpuLoad = entry.getValue();

					long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, minute, "minutes");
					LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());
					String minuteLabel = dateTime.format(DateTimeFormatter.ofPattern("HH:mm"));

					hourLabels.add(minuteLabel);
					cpuValues.add(cpuLoad);

					// Color coding: green for low, yellow for medium, red for high CPU usage
					lineColor = getCPULoadColor(cpuLoad, 0.8f);
				}
			} else {
				// Fallback to seconds data if available
				Map<Integer, Float> secondsCPUData = resourceMeasurements.getSecondsCPUCoresUsage();

				if (!secondsCPUData.isEmpty()) {
					cd.setTitle("System CPU Load - Past 60 Seconds");

					for (Map.Entry<Integer, Float> entry : secondsCPUData.entrySet()) {
						int second = entry.getKey();
						float cpuLoad = entry.getValue();

						long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, second, "seconds");
						LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis),
								ZoneId.systemDefault());
						String secondLabel = dateTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"));

						hourLabels.add(secondLabel);
						cpuValues.add(cpuLoad);

						lineColor = getCPULoadColor(cpuLoad, 0.8f);
					}
				}
			}
		} else {
			// Use hourly data with proper timestamps
			for (Map.Entry<Integer, Float> entry : hoursCPUData.entrySet()) {
				int hour = entry.getKey();
				float cpuLoad = entry.getValue();

				// Calculate actual timestamp for this hour index
				long timestampMillis = calculateTimestampForIndex(monitoringStartTime, currentTime, hour, "hours");
				LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestampMillis), ZoneId.systemDefault());
				String hourLabel = dateTime.format(DateTimeFormatter.ofPattern("MM/dd HH:00"));

				hourLabels.add(hourLabel);
				cpuValues.add(cpuLoad);

				// Color coding based on CPU load
				lineColor = getCPULoadColor(cpuLoad, 0.8f);
			}
		}

		// If no data at all, show a placeholder
		if (hourLabels.isEmpty()) {
			hourLabels.add("No Data");
			cpuValues.add(0);
			cd.setTitle("System CPU Load - No Data Available");
		}

		// Set the chart data
		cd.setLabels(hourLabels);
		cd.setValues(cpuValues);
		cd.setBackground(lineColor);
		cd.setBorder(lineColor);

		return cd;
	}

	private long calculateTimestampForIndex(long startTime, long currentTime, int index, String timePeriod) {
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
	 * Get color based on CPU load level
	 * 
	 * @param cpuLoad CPU load value (in cores)
	 * @param saturation Color saturation (0.0 to 1.0)
	 * @return Color representing the CPU load level
	 */
	private Color getCPULoadColor(float cpuLoad, float saturation) {
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