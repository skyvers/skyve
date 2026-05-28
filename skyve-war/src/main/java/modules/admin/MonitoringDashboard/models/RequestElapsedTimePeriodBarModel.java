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
	/**
	 * Executes getChartTitle.
	 * @param requestDescription the requestDescription value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String requestDescription) {
		return "Elapsed Time Averages - " + requestDescription;
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "Average Elapsed Time (ms)";
	}

	/**
	 * Executes getChartColour.
	 * @return the result
	 */
	@Override
	protected Color getChartColour() {
		return new Color(52, 152, 219); // Blue
	}

	/**
	 * Executes extractDataForTimePeriod.
	 * @param measurements the measurements value
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractMillisForTimePeriod(measurements, period);
	}
}
