package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document RAM delta monitoring.
 * Shows memory usage changes for create operations on selected documents.
 */
public class DocumentRAMDeltaModel extends AbstractDocumentChartModel {

	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Create Request RAM Delta - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "RAM Delta (MB)";
	}

	@Override
	protected Color getChartColor() {
		return Color.ORANGE;
	}

	@Override
	protected String getRequestKey(String selectedDocument) {
		return "C" + selectedDocument;
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