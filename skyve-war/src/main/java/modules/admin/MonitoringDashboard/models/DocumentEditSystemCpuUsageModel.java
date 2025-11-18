package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document edit System CPU Usage monitoring.
 * Shows system load usage for edit operations on selected documents.
 */
public class DocumentEditSystemCpuUsageModel extends AbstractDocumentChartModel {
	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request System CPU Usage - " + selectedDocument;
	}

	@Override
	protected String getChartLabel() {
		return "System CPU usage (%)";
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
		return RequestListModel.extractSystemCpuUsageForTimePeriod(measurements, period);
	}
}
