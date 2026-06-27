package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.monitoring.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Line chart model for system CPU usage trend data in the monitoring dashboard.
 */
public class SystemCpuUsageModel extends AbstractSystemResourceModel {
	/**
	 * Returns the chart dataset label.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "System CPU Usage (%)";
	}

	/**
	 * Returns the chart title for the selected period.
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(Period period) {
		return "System CPU Usage - " + period.toLocalisedDescription();
	}

	/**
	 * Selects system CPU usage telemetry aligned to the chosen period.
	 * @param resourceMeasurements the resourceMeasurements value
	 * @param period the period value
	 * @return the result
	 */
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

	/**
	 * Applies usage-based line colouring for CPU charts.
	 * @param cd the cd value
	 * @param values the values value
	 */
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
