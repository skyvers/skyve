package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for query elapsed time monitoring.
 * Shows response times for query model operations on selected queries.
 */
public class QueryElapsedTimeModel extends AbstractQueryChartModel {
	@Override
	protected String getChartTitle(String selectedQuery) {
		return "Query Request Elapsed Time - " + selectedQuery;
	}

	@Override
	protected String getChartLabel() {
		return "Elapsed Time (ms)";
	}

	@Override
	protected Color getChartColor() {
		return Color.BLUE;
	}

	@Override
	protected Map<Integer, Integer> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractMillisForTimePeriod(measurements, period);
	}
}