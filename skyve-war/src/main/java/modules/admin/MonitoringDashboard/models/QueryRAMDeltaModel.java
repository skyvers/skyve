package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for query RAM delta monitoring.
 * Shows memory usage changes for query list operations on selected queries.
 */
public class QueryRAMDeltaModel extends AbstractQueryChartModel {

	@Override
	protected String getChartTitle(String selectedQuery) {
		return "Query List RAM Delta - " + selectedQuery;
	}

	@Override
	protected String getChartLabel() {
		return "RAM Delta (%)";
	}

	@Override
	protected Color getChartColor() {
		return Color.ORANGE;
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
			case pastMinute:
				return measurements.getSecondsRAMPercentageDelta();
			case pastHour:
				return measurements.getMinutesRAMPercentageDelta();
			case pastDay:
				return measurements.getHoursRAMPercentageDelta();
			case pastWeek:
				return measurements.getDaysRAMPercentageDelta();
			case pastYear:
				return measurements.getWeeksRAMPercentageDelta();
			default:
				return measurements.getHoursRAMPercentageDelta();
		}
	}
}