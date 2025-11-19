package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document edit CPU Utilisation monitoring.
 * Shows CPU utilisation for edit operations on selected documents.
 */
public class DocumentEditCpuUtilisationModel extends AbstractDocumentChartModel {
	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request CPU Utilisation - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "CPU Utilisation (%)";
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
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractCpuUtilisationForTimePeriod(measurements, period);
	}
}
