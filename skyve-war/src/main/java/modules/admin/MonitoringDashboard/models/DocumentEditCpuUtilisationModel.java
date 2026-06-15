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
	/**
	 * Executes getChartTitle.
	 * @param selectedDocument the selectedDocument value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Edit Request CPU Utilisation - " + selectedDocument;
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "CPU Utilisation (%)";
	}

	/**
	 * Executes getChartColor.
	 * @return the result
	 */
	@Override
	protected Color getChartColor() {
		return Color.MAGENTA;
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
		return RequestListModel.extractCpuUtilisationForTimePeriod(measurements, period);
	}
}
