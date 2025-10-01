package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

/**
 * Bar chart model for request RAM usage period averages.
 * Shows average RAM usage deltas across different time periods (seconds, minutes, hours, days, weeks).
 */
public class RequestRAMUsagePeriodBarModel extends AbstractRequestPeriodBarChartModel {

	@Override
	protected String getChartTitle(String requestDescription) {
		return "RAM Usage Period Averages - " + requestDescription;
	}

	@Override
	protected String getChartLabel() {
		return "Average RAM Usage Delta (%)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(46, 204, 113); // Green
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, String timePeriod) {
		return switch (timePeriod) {
			case "seconds" -> measurements.getSecondsRAMPercentageDelta();
			case "minutes" -> measurements.getMinutesRAMPercentageDelta();
			case "hours" -> measurements.getHoursRAMPercentageDelta();
			case "days" -> measurements.getDaysRAMPercentageDelta();
			case "weeks" -> measurements.getWeeksRAMPercentageDelta();
			default -> measurements.getHoursRAMPercentageDelta();
		};
	}

	@Override
	protected boolean isSignificantValue(Number value) {
		// For RAM usage deltas, include any non-zero values (can be positive or negative)
		return value != null && value.doubleValue() != 0.0;
	}
}