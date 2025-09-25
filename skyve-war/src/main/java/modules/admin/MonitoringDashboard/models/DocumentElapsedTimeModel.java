package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

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