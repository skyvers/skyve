package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

/**
 * Bar chart model for request CPU time period averages.
 * Shows average CPU time deltas across different time periods (seconds, minutes, hours, days, weeks).
 */
public class RequestCPUTimePeriodBarModel extends AbstractRequestPeriodBarChartModel {

	@Override
	protected String getChartTitle(String requestDescription) {
		return "CPU Time Period Averages - " + requestDescription;
	}

	@Override
	protected String getChartLabel() {
		return "Average CPU Time Delta (ms)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(231, 76, 60); // Red
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, String timePeriod) {
		return switch (timePeriod) {
			case "seconds" -> measurements.getSecondsCPUTimeDelta();
			case "minutes" -> measurements.getMinutesCPUTimeDelta();
			case "hours" -> measurements.getHoursCPUTimeDelta();
			case "days" -> measurements.getDaysCPUTimeDelta();
			case "weeks" -> measurements.getWeeksCPUTimeDelta();
			default -> measurements.getHoursCPUTimeDelta();
		};
	}
}