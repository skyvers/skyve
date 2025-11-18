package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for query System CPU usage monitoring.
 * Shows system load changes for model operations on selected queries.
 */
public class QuerySystemCpuUsageModel extends AbstractQueryChartModel {
	@Override
	protected String getChartTitle(String selectedQuery) {
		return "Query Request Heap RAM Usage - " + selectedQuery;
	}

	@Override
	protected String getChartLabel() {
		return "Heap RAM Usage (%)";
	}

	@Override
	protected Color getChartColor() {
		return Color.ORANGE;
	}

	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractHeapRamUsageForTimePeriod(measurements, period);
	}
}