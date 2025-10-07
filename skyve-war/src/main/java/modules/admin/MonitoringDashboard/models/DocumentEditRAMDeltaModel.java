package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document edit RAM delta monitoring.
 * Shows memory usage changes for edit operations on selected documents.
 */
public class DocumentEditRAMDeltaModel extends AbstractDocumentChartModel {

	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request RAM Delta - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "RAM Delta (%)";
	}

	@Override
	protected Color getChartColor() {
		return Color.CYAN;
	}

	@Override
	protected String getRequestKey(String selectedDocument) {
		return "E" + selectedDocument;
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case currentMinute:
				return measurements.getSecondsRAMPercentageDelta();
			case currentHour:
				return measurements.getMinutesRAMPercentageDelta();
			case currentDay:
				return measurements.getHoursRAMPercentageDelta();
			case currentWeek:
				return measurements.getDaysRAMPercentageDelta();
			case currentYear:
				return measurements.getWeeksRAMPercentageDelta();
			default:
				return measurements.getHoursRAMPercentageDelta();
		}
	}
}