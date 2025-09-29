package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

/**
 * Chart model for document CPU delta monitoring.
 * Shows CPU usage changes for create operations on selected documents.
 */
public class DocumentCPUDeltaModel extends AbstractDocumentChartModel {

	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Create Request CPU Delta - " + selectedDocument;
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
	protected String getRequestKey(String selectedDocument) {
		return "C" + selectedDocument;
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, String timePeriod) {
		switch (timePeriod) {
			case "hours":
				return measurements.getHoursCPUTimeDelta();
			case "minutes":
				return measurements.getMinutesCPUTimeDelta();
			case "seconds":
				return measurements.getSecondsCPUTimeDelta();
			default:
				return measurements.getHoursCPUTimeDelta();
		}
	}
}