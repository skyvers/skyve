package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Bar chart model for request CPU Utilisation period averages.
 * Shows average CPU Utilisation across different time periods (seconds, minutes, hours, days, weeks).
 */
public class RequestCpuUtilisationPeriodBarModel extends AbstractRequestPeriodBarChartModel {
	@Override
	protected String getChartTitle(String requestDescription) {
		return "CPU Utilsation Averages - " + requestDescription;
	}

	@Override
	protected String getChartLabel() {
		return "Average CPU Utilisation (ms)";
	}

	@Override
	protected Color getChartColour() {
		return new Color(231, 76, 60); // Red
	}

	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractCpuUtilisationForTimePeriod(measurements, period);
	}
}
