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
	@Override
	protected String getChartTitle(String requestKey) {
		return "Request Elapsed Time - " + getRequestDescription(getBean());
	}

	@Override
	protected String getChartLabel() {
		return "Elapsed Time (ms)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(52, 152, 219); // Blue
	}

	@Override
	protected Map<Integer, Integer> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractMillisForTimePeriod(measurements, period);
	}
}
