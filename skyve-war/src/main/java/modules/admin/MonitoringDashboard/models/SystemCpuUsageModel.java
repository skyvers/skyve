package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.monitoring.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

public class SystemCpuUsageModel extends AbstractSystemResourceModel {
	@Override
	protected String getChartLabel() {
		return "System CPU Usage (%)";
	}

	@Override
	protected String getChartTitle(Period period) {
		return "System CPU Usage - " + period.toLocalisedDescription();
	}

	@Override
	protected Map<Integer, Float> getResourceDataForPeriod(ResourceMeasurements resourceMeasurements, Period period) {
		switch (period) {
			case currentMinute:
				return resourceMeasurements.getSecondsSystemCpuUsageUsage();
			case currentHour:
				return resourceMeasurements.getMinutesSystemCpuUsageUsage();
			case currentDay:
				return resourceMeasurements.getHoursSystemCpuUsage();
			case currentWeek:
				return resourceMeasurements.getDaysSystemCpuUsage();
			case currentYear:
				return resourceMeasurements.getWeeksSystemCpuUsage();
			default:
				return resourceMeasurements.getHoursSystemCpuUsage();
		}
	}

	@Override
	protected void setChartColors(ChartData cd, List<Number> values) {
		Color lineColour = Color.LIGHT_GRAY;
		if (! values.isEmpty()) {
			// Use the last value for colour determination
			Number lastValue = values.get(values.size() - 1);
			float cpuLoad = lastValue.floatValue();
			lineColour = getCPULoadColor(cpuLoad, 0.8f);
		}
		cd.setBackground(lineColour);
		cd.setBorder(lineColour);
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

		if (cpuLoad < 50f) {
			// Low load: Green
			hue = 0.33f; // Green
		}
		else if (cpuLoad < 70f) {
			// Medium load: Yellow
			hue = 0.17f; // Yellow
		}
		else if (cpuLoad < 85f) {
			// High load: Orange
			hue = 0.08f; // Orange
		}
		else {
			// Very high load: Red
			hue = 0f; // Red
		}

		return Color.getHSBColor(hue, saturation, 0.9f);
	}
}