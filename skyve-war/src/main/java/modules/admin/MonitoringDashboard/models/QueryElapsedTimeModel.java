package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

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
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case currentMinute:
				return measurements.getSecondsMillis();
			case currentHour:
				return measurements.getMinutesMillis();
			case currentDay:
				return measurements.getHoursMillis();
			case currentWeek:
				return measurements.getDaysMillis();
			case currentYear:
				return measurements.getWeeksMillis();
			default:
				return measurements.getHoursMillis();
		}
	}
}