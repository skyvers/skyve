package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

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
	protected Map<Integer, Integer> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractMillisForTimePeriod(measurements, period);
	}
}