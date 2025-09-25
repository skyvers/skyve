package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

/**
 * Chart model for query elapsed time monitoring.
 * Shows response times for query list operations on selected queries.
 */
public class QueryElapsedTimeModel extends AbstractQueryChartModel {

	@Override
	protected String getChartTitle(String selectedQuery) {
		return "Query List Elapsed Time - " + selectedQuery;
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
	protected String getRequestKey(String selectedQuery) {
		// Convert module.queryName to Qmodule^queryName format
		if (selectedQuery != null && selectedQuery.contains(".")) {
			return "Q" + selectedQuery.replace(".", "^");
		}
		return "Q" + selectedQuery;
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, String timePeriod) {
		switch (timePeriod) {
			case "hours":
				return measurements.getHoursMillis();
			case "minutes":
				return measurements.getMinutesMillis();
			case "seconds":
				return measurements.getSecondsMillis();
			default:
				return measurements.getHoursMillis();
		}
	}
}