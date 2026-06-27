package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for query CPU Utilisation monitoring.
 * Shows request CPU utilisation for query operations on selected queries.
 */
public class QueryCpuUtilisationModel extends AbstractQueryChartModel {
	/**
	 * Executes getChartTitle.
	 * @param selectedQuery the selectedQuery value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String selectedQuery) {
		return "Query Request CPU Utilisation - " + selectedQuery;
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
		return Color.GREEN;
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
