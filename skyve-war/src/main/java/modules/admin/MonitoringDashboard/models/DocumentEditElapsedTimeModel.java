package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document edit elapsed time monitoring.
 * Shows response times for edit operations on selected documents.
 */
public class DocumentEditElapsedTimeModel extends AbstractDocumentChartModel {

	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request Elapsed Time - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "Elapsed Time (ms)";
	}

	@Override
	protected Color getChartColor() {
		return Color.RED;
	}

	@Override
	protected String getRequestKey(String selectedDocument) {
		return "E" + selectedDocument;
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case pastMinute:
				return measurements.getSecondsMillis();
			case pastHour:
				return measurements.getMinutesMillis();
			case pastDay:
				return measurements.getHoursMillis();
			case pastWeek:
				return measurements.getDaysMillis();
			case pastYear:
				return measurements.getWeeksMillis();
			default:
				return measurements.getHoursMillis();
		}
	}
}