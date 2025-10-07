package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

public class SystemRAMUsageModel extends AbstractSystemResourceModel {

	@Override
	protected String getChartLabel() {
		return "RAM Usage (%)";
	}

	@Override
	protected String getChartTitle(Period period) {
		return "System RAM Usage - " + period.toLocalisedDescription();
	}

	@Override
	protected Map<Integer, Float> getResourceDataForPeriod(ResourceMeasurements resourceMeasurements, Period period) {
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

	@Override
	protected void setChartColors(ChartData cd, List<Number> values) {
		Color lineColor = Color.LIGHT_GRAY;
		if (!values.isEmpty()) {
			// Use the last value for color determination, or could iterate through all
			Number lastValue = values.get(values.size() - 1);
			float ramUsage = lastValue.floatValue();
			lineColor = getRAMUsageColor(ramUsage, 0.8f);
		}
		cd.setBackground(lineColor);
		cd.setBorder(lineColor);
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