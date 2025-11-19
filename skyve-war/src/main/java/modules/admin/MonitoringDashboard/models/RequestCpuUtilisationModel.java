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
	@Override
	protected String getChartTitle(String requestKey) {
		return "Request CPU Utilisation - " + getRequestDescription(getBean());
	}

	@Override
	protected String getChartLabel() {
		return "CPU Utilisation (%)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(231, 76, 60); // Red
	}

	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractCpuUtilisationForTimePeriod(measurements, period);
	}
}