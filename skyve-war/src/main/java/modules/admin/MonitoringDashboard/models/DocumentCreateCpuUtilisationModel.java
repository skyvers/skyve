package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document CPU Utilisation monitoring.
 * Shows Request CPU Utilisation for create operations on selected documents.
 */
public class DocumentCreateCpuUtilisationModel extends AbstractDocumentChartModel {

	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Create Request CPU Utilisation - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "CPU Utilisation (%)";
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
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractCpuUtilisationForTimePeriod(measurements, period);
	}
}