package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for request CPU Utilisation monitoring.
 * Shows CPU utilisation for the selected request type/module/document/component combination.
 */
public class RequestCpuUtilisationModel extends AbstractRequestChartModel {
	/**
	 * Executes getChartTitle.
	 * @param requestKey the requestKey value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String requestKey) {
		return "Request CPU Utilisation - " + getRequestDescription(getBean());
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "CPU Utilisation (%)";
	}

	/**
	 * Executes getChartColor.
	 * @return the result
	 */
	@Override
	protected Color getChartColor() {
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
