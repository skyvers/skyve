package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

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
		return "CPU Time Delta (ms)";
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
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case currentMinute:
				return measurements.getSecondsCPUTimeDelta();
			case currentHour:
				return measurements.getMinutesCPUTimeDelta();
			case currentDay:
				return measurements.getHoursCPUTimeDelta();
			case currentWeek:
				return measurements.getDaysCPUTimeDelta();
			case currentYear:
				return measurements.getWeeksCPUTimeDelta();
			default:
				return measurements.getHoursCPUTimeDelta();
		}
	}
}