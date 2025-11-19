package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for query Heap RAM usage monitoring.
 * Shows memory usage changes for model operations on selected queries.
 */
public class QueryHeapRamUsageModel extends AbstractQueryChartModel {
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