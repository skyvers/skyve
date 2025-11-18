package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Bar chart model for request elapsed time period averages.
 * Shows average response times across different time periods (seconds, minutes, hours, days, weeks).
 */
public class RequestElapsedTimePeriodBarModel extends AbstractRequestPeriodBarChartModel {
	@Override
	protected String getChartTitle(String requestDescription) {
		return "Elapsed Time Averages - " + requestDescription;
	}

	@Override
	protected String getChartLabel() {
		return "Average Elapsed Time (ms)";
	}

	@Override
	protected Color getChartColour() {
		return new Color(52, 152, 219); // Blue
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractMillisForTimePeriod(measurements, period);
	}
}
