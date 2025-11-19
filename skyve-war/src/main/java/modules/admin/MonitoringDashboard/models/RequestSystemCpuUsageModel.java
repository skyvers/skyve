package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for request System CPU usage monitoring.
 * Shows System CPU usage for the selected request type/module/document/component combination.
 */
public class RequestSystemCpuUsageModel extends AbstractRequestChartModel {
	@Override
	protected String getChartTitle(String requestKey) {
		return "Request System CPU Usage - " + getRequestDescription(getBean());
	}

	@Override
	protected String getChartLabel() {
		return "System CPU Usage (%)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(46, 204, 113); // Green
	}

	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractSystemCpuUsageForTimePeriod(measurements, period);
	}
}
