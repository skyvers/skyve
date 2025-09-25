package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

/**
 * Chart model for document edit CPU delta monitoring.
 * Shows CPU usage changes for edit operations on selected documents.
 */
public class DocumentEditCPUDeltaModel extends AbstractDocumentChartModel {

	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request CPU Delta - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "CPU Delta (cores)";
	}

	@Override
	protected Color getChartColor() {
		return Color.MAGENTA;
	}

	@Override
	protected String getRequestKey(String selectedDocument) {
		return "E" + selectedDocument;
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