package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

public class SystemCPULoadModel extends AbstractSystemResourceModel {

	@Override
	protected String getChartLabel() {
		return "CPU Load (cores)";
	}

	@Override
	protected String getChartTitle(Period period) {
		return "System CPU Load - " + period.toLocalisedDescription();
	}

	@Override
	protected Map<Integer, Float> getResourceDataForPeriod(ResourceMeasurements resourceMeasurements, Period period) {
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

	@Override
	protected void setChartColors(ChartData cd, List<Number> values) {
		Color lineColor = Color.LIGHT_GRAY;
		if (!values.isEmpty()) {
			// Use the last value for color determination
			Number lastValue = values.get(values.size() - 1);
			float cpuLoad = lastValue.floatValue();
			lineColor = getCPULoadColor(cpuLoad, 0.8f);
		}
		cd.setBackground(lineColor);
		cd.setBorder(lineColor);
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