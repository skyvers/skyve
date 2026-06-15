package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for request elapsed time monitoring.
 * Shows response times for the selected request type/module/document/component combination.
 */
public class RequestElapsedTimeModel extends AbstractRequestChartModel {
	/**
	 * Executes getChartTitle.
	 * @param requestKey the requestKey value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String requestKey) {
		return "Request Elapsed Time - " + getRequestDescription(getBean());
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "Elapsed Time (ms)";
	}

	/**
	 * Executes getChartColor.
	 * @return the result
	 */
	@Override
	protected Color getChartColor() {
		return new Color(52, 152, 219); // Blue
	}

	/**
	 * Executes extractDataForTimePeriod.
	 * @param measurements the measurements value
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected Map<Integer, Integer> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractMillisForTimePeriod(measurements, period);
	}
}
