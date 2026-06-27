package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.monitoring.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Line chart model for system heap RAM usage trend data in the monitoring dashboard.
 */
public class SystemHeapRamUsageModel extends AbstractSystemResourceModel {
	/**
	 * Returns the chart dataset label.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "Heap RAM Usage (%)";
	}

	/**
	 * Returns the chart title for the selected period.
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(Period period) {
		return "Heap RAM Usage - " + period.toLocalisedDescription();
	}

	/**
	 * Selects heap RAM usage telemetry aligned to the chosen period.
	 * @param resourceMeasurements the resourceMeasurements value
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected Map<Integer, Float> getResourceDataForPeriod(ResourceMeasurements resourceMeasurements, Period period) {
		switch (period) {
			case currentMinute:
				return resourceMeasurements.getSecondsHeapRamUsage();
			case currentHour:
				return resourceMeasurements.getMinutesHeapRamUsage();
			case currentDay:
				return resourceMeasurements.getHoursHeapRamUsage();
			case currentWeek:
				return resourceMeasurements.getDaysHeapRamUsage();
			case currentYear:
				return resourceMeasurements.getWeeksHeapRamUsage();
			default:
				return resourceMeasurements.getHoursHeapRamUsage();
		}
	}

	/**
	 * Applies usage-based line colouring for RAM charts.
	 * @param cd the cd value
	 * @param values the values value
	 */
	@Override
	protected void setChartColors(ChartData cd, List<Number> values) {
		Color lineColour = Color.LIGHT_GRAY;
		if (!values.isEmpty()) {
			// Use the last value for color determination, or could iterate through all
			Number lastValue = values.get(values.size() - 1);
			float ramUsage = lastValue.floatValue();
			lineColour = getRAMUsageColor(ramUsage, 0.8f);
		}
		cd.setBackground(lineColour);
		cd.setBorder(lineColour);
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

		if (ramUsage < 50f) {
			// Low usage: Green
			hue = 0.33f; // Green
		} else if (ramUsage < 70f) {
			// Medium usage: Yellow
			hue = 0.17f; // Yellow
		} else if (ramUsage < 85f) {
			// High usage: Orange
			hue = 0.08f; // Orange
		} else {
			// Critical usage: Red
			hue = 0f; // Red
		}

		return Color.getHSBColor(hue, saturation, 0.9f);
	}
}
