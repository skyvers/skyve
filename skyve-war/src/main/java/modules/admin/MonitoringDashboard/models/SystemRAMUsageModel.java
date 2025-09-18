package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
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
		cd.setTitle("System RAM Usage - Past 24 Hours");
		cd.setLabel("RAM Usage (%)");

		// Get system resource measurements
		ResourceMeasurements resourceMeasurements = Monitoring.getResourceMeasurements();

		// Data structures to hold our chart data
		List<String> hourLabels = new ArrayList<>();
		List<Number> ramValues = new ArrayList<>();
		List<Color> backgrounds = new ArrayList<>();
		List<Color> borders = new ArrayList<>();

		// Get hourly RAM usage data (past 24 hours)
		Map<Integer, Float> hoursRAMData = resourceMeasurements.getHoursRAMPercentage();

		// If no hourly data, fallback to minutes data
		if (hoursRAMData.isEmpty()) {
			Map<Integer, Float> minutesRAMData = resourceMeasurements.getMinutesRAMPercentage();
			
			if (!minutesRAMData.isEmpty()) {
				cd.setTitle("System RAM Usage - Past 60 Minutes");
				
				for (Map.Entry<Integer, Float> entry : minutesRAMData.entrySet()) {
					int minute = entry.getKey();
					float ramUsage = entry.getValue();
					
					hourLabels.add("Min " + minute);
					ramValues.add(ramUsage);
					
					// Color coding: green for low, yellow for medium, red for high RAM usage
					Color backgroundColor = getRAMUsageColor(ramUsage, 0.6f);
					Color borderColor = getRAMUsageColor(ramUsage, 0.9f);
					
					backgrounds.add(backgroundColor);
					borders.add(borderColor);
				}
			} else {
				// Fallback to seconds data if available
				Map<Integer, Float> secondsRAMData = resourceMeasurements.getSecondsRAMPercentage();
				
				if (!secondsRAMData.isEmpty()) {
					cd.setTitle("System RAM Usage - Past 60 Seconds");
					
					for (Map.Entry<Integer, Float> entry : secondsRAMData.entrySet()) {
						int second = entry.getKey();
						float ramUsage = entry.getValue();
						
						hourLabels.add("Sec " + second);
						ramValues.add(ramUsage);
						
						Color backgroundColor = getRAMUsageColor(ramUsage, 0.6f);
						Color borderColor = getRAMUsageColor(ramUsage, 0.9f);
						
						backgrounds.add(backgroundColor);
						borders.add(borderColor);
					}
				}
			}
		} else {
			// Use hourly data
			for (Map.Entry<Integer, Float> entry : hoursRAMData.entrySet()) {
				int hour = entry.getKey();
				float ramUsage = entry.getValue();
				
				// Format hour label (0-23)
				String hourLabel = String.format("%02d:00", hour);
				hourLabels.add(hourLabel);
				ramValues.add(ramUsage);
				
				// Color coding based on RAM usage
				Color backgroundColor = getRAMUsageColor(ramUsage, 0.6f);
				Color borderColor = getRAMUsageColor(ramUsage, 0.9f);
				
				backgrounds.add(backgroundColor);
				borders.add(borderColor);
			}
		}

		// If no data at all, show a placeholder
		if (hourLabels.isEmpty()) {
			hourLabels.add("No Data");
			ramValues.add(0);
			backgrounds.add(Color.LIGHT_GRAY);
			borders.add(Color.GRAY);
			cd.setTitle("System RAM Usage - No Data Available");
		}

		// Set the chart data
		cd.setLabels(hourLabels);
		cd.setValues(ramValues);
		cd.setBackgrounds(backgrounds);
		cd.setBorders(borders);

		return cd;
	}

	/**
	 * Get color based on RAM usage percentage
	 * @param ramUsage RAM usage percentage (0-100)
	 * @param saturation Color saturation (0.0 to 1.0)
	 * @return Color representing the RAM usage level
	 */
	private Color getRAMUsageColor(float ramUsage, float saturation) {
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