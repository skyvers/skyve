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

public class SystemCPULoadModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		ChartData cd = new ChartData();
		cd.setTitle("System CPU Load - Past 24 Hours");
		cd.setLabel("CPU Load (cores)");

		// Get system resource measurements
		ResourceMeasurements resourceMeasurements = Monitoring.getResourceMeasurements();

		// Data structures to hold our chart data
		List<String> hourLabels = new ArrayList<>();
		List<Number> cpuValues = new ArrayList<>();
		List<Color> backgrounds = new ArrayList<>();
		List<Color> borders = new ArrayList<>();

		// Get hourly CPU usage data (past 24 hours)
		Map<Integer, Float> hoursCPUData = resourceMeasurements.getHoursCPUCoresUsage();

		// If no hourly data, fallback to minutes data
		if (hoursCPUData.isEmpty()) {
			Map<Integer, Float> minutesCPUData = resourceMeasurements.getMinutesCPUCoresUsage();
			
			if (!minutesCPUData.isEmpty()) {
				cd.setTitle("System CPU Load - Past 60 Minutes");
				
				for (Map.Entry<Integer, Float> entry : minutesCPUData.entrySet()) {
					int minute = entry.getKey();
					float cpuLoad = entry.getValue();
					
					hourLabels.add("Min " + minute);
					cpuValues.add(cpuLoad);
					
					// Color coding: green for low, yellow for medium, red for high CPU usage
					Color backgroundColor = getCPULoadColor(cpuLoad, 0.6f);
					Color borderColor = getCPULoadColor(cpuLoad, 0.9f);
					
					backgrounds.add(backgroundColor);
					borders.add(borderColor);
				}
			} else {
				// Fallback to seconds data if available
				Map<Integer, Float> secondsCPUData = resourceMeasurements.getSecondsCPUCoresUsage();
				
				if (!secondsCPUData.isEmpty()) {
					cd.setTitle("System CPU Load - Past 60 Seconds");
					
					for (Map.Entry<Integer, Float> entry : secondsCPUData.entrySet()) {
						int second = entry.getKey();
						float cpuLoad = entry.getValue();
						
						hourLabels.add("Sec " + second);
						cpuValues.add(cpuLoad);
						
						Color backgroundColor = getCPULoadColor(cpuLoad, 0.6f);
						Color borderColor = getCPULoadColor(cpuLoad, 0.9f);
						
						backgrounds.add(backgroundColor);
						borders.add(borderColor);
					}
				}
			}
		} else {
			// Use hourly data
			for (Map.Entry<Integer, Float> entry : hoursCPUData.entrySet()) {
				int hour = entry.getKey();
				float cpuLoad = entry.getValue();
				
				// Format hour label (0-23)
				String hourLabel = String.format("%02d:00", hour);
				hourLabels.add(hourLabel);
				cpuValues.add(cpuLoad);
				
				// Color coding based on CPU load
				Color backgroundColor = getCPULoadColor(cpuLoad, 0.6f);
				Color borderColor = getCPULoadColor(cpuLoad, 0.9f);
				
				backgrounds.add(backgroundColor);
				borders.add(borderColor);
			}
		}

		// If no data at all, show a placeholder
		if (hourLabels.isEmpty()) {
			hourLabels.add("No Data");
			cpuValues.add(0);
			backgrounds.add(Color.LIGHT_GRAY);
			borders.add(Color.GRAY);
			cd.setTitle("System CPU Load - No Data Available");
		}

		// Set the chart data
		cd.setLabels(hourLabels);
		cd.setValues(cpuValues);
		cd.setBackgrounds(backgrounds);
		cd.setBorders(borders);

		return cd;
	}

	/**
	 * Get color based on CPU load level
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