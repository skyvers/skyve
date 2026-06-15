package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for request Heap RAM usage monitoring.
 * Shows Heap RAM usage for the selected request type/module/document/component combination.
 */
public class RequestHeapRamUsageModel extends AbstractRequestChartModel {
	/**
	 * Executes getChartTitle.
	 * @param requestKey the requestKey value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String requestKey) {
		return "Request Heap RAM Usage - " + getRequestDescription(getBean());
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "Heap RAM Usage (%)";
	}

	/**
	 * Executes getChartColor.
	 * @return the result
	 */
	@Override
	protected Color getChartColor() {
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
