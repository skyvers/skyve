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
	/**
	 * Executes getChartTitle.
	 * @param selectedQuery the selectedQuery value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String selectedQuery) {
		return "Query Request Elapsed Time - " + selectedQuery;
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
		return Color.BLUE;
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
