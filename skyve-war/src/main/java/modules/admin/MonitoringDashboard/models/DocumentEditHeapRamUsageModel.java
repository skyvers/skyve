package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document edit Heap RAM Usage monitoring.
 * Shows memory usage for edit operations on selected documents.
 */
public class DocumentEditHeapRamUsageModel extends AbstractDocumentChartModel {
	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request Heap RAM Usage - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "Heap RAM usage (%)";
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
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractHeapRamUsageForTimePeriod(measurements, period);
	}
}
