package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Bar chart model for request Heap RAM usage period averages.
 * Shows average Heap RAM usages across different time periods (seconds, minutes, hours, days, weeks).
 */
public class RequestHeapRamUsagePeriodBarModel extends AbstractRequestPeriodBarChartModel {
	/**
	 * Executes getChartTitle.
	 * @param requestDescription the requestDescription value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String requestDescription) {
		return "Heap RAM Usage Averages - " + requestDescription;
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "Average Heap RAM Usage (%)";
	}

	/**
	 * Executes getChartColour.
	 * @return the result
	 */
	@Override
	protected Color getChartColour() {
		return new Color(46, 204, 113); // Green
	}

	/**
	 * Executes extractDataForTimePeriod.
	 * @param measurements the measurements value
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractHeapRamUsageForTimePeriod(measurements, period);
	}
}
