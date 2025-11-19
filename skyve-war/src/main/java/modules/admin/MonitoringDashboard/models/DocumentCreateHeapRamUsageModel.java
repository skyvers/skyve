package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document RAM Usage monitoring.
 * Shows memory usage changes for create operations on selected documents.
 */
public class DocumentCreateHeapRamUsageModel extends AbstractDocumentChartModel {
	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Create Request Heap RAM Usage - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "Heap RAM Usage (%)";
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
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractHeapRamUsageForTimePeriod(measurements, period);
	}
}
