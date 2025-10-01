package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

/**
 * Bar chart model for request elapsed time period averages.
 * Shows average response times across different time periods (seconds, minutes, hours, days, weeks).
 */
public class RequestElapsedTimePeriodBarModel extends AbstractRequestPeriodBarChartModel {

	@Override
	protected String getChartTitle(String requestDescription) {
		return "Elapsed Time Period Averages - " + requestDescription;
	}

	@Override
	protected String getChartLabel() {
		return "Average Elapsed Time (ms)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(52, 152, 219); // Blue
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, String timePeriod) {
		return switch (timePeriod) {
			case "seconds" -> measurements.getSecondsMillis();
			case "minutes" -> measurements.getMinutesMillis();
			case "hours" -> measurements.getHoursMillis();
			case "days" -> measurements.getDaysMillis();
			case "weeks" -> measurements.getWeeksMillis();
			default -> measurements.getHoursMillis();
		};
	}

	@Override
	protected boolean isSignificantValue(Number value) {
		// For elapsed time, only include positive values (response times should be > 0)
		return value != null && value.doubleValue() > 0.0;
	}
}