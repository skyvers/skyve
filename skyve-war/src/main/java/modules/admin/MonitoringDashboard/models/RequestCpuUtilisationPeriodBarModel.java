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
	/**
	 * Executes getChartTitle.
	 * @param requestDescription the requestDescription value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String requestDescription) {
		return "CPU Utilsation Averages - " + requestDescription;
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "Average CPU Utilisation (ms)";
	}

	/**
	 * Executes getChartColour.
	 * @return the result
	 */
	@Override
	protected Color getChartColour() {
		return new Color(231, 76, 60); // Red
	}

	/**
	 * Executes extractDataForTimePeriod.
	 * @param measurements the measurements value
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractCpuUtilisationForTimePeriod(measurements, period);
	}
}
