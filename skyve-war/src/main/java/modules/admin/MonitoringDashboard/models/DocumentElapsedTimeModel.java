package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document elapsed time monitoring.
 * Shows response times for create operations on selected documents.
 */
public class DocumentElapsedTimeModel extends AbstractDocumentChartModel {

	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Create Request Elapsed Time - " + selectedDocument;
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
	protected String getRequestKey(String selectedDocument) {
		return "C" + selectedDocument;
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