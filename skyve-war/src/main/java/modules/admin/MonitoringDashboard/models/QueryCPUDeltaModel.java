package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

/**
 * Chart model for query CPU delta monitoring.
 * Shows CPU usage changes for query list operations on selected queries.
 */
public class QueryCPUDeltaModel extends AbstractQueryChartModel {

	@Override
	protected String getChartTitle(String selectedQuery) {
		return "Query List CPU Delta - " + selectedQuery;
	}

	@Override
	protected String getChartLabel() {
		return "CPU Delta (cores)";
	}

	@Override
	protected Color getChartColor() {
		return Color.GREEN;
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
				return measurements.getHoursCPUCoresDelta();
			case "minutes":
				return measurements.getMinutesCPUCoresDelta();
			case "seconds":
				return measurements.getSecondsCPUCoresDelta();
			default:
				return measurements.getHoursCPUCoresDelta();
		}
	}
}