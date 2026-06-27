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
	/**
	 * Executes getChartTitle.
	 * @param selectedDocument the selectedDocument value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request System CPU Usage - " + selectedDocument;
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "System CPU usage (%)";
	}

	/**
	 * Executes getChartColor.
	 * @return the result
	 */
	@Override
	protected Color getChartColor() {
		return Color.CYAN;
	}

	/**
	 * Executes getRequestKey.
	 * @param selectedDocument the selectedDocument value
	 * @return the result
	 */
	@Override
	protected String getRequestKey(String selectedDocument) {
		return "E" + selectedDocument;
	}

	/**
	 * Executes extractDataForTimePeriod.
	 * @param measurements the measurements value
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractSystemCpuUsageForTimePeriod(measurements, period);
	}
}
