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
	@Override
	protected String getChartTitle(String requestKey) {
		return "Request Heap RAM Usage - " + getRequestDescription(getBean());
	}

	@Override
	protected String getChartLabel() {
		return "Heap RAM Usage (%)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(46, 204, 113); // Green
	}

	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractHeapRamUsageForTimePeriod(measurements, period);
	}
}