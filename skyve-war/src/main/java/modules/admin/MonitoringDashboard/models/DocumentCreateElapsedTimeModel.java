package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for document elapsed time monitoring.
 * Shows response times for create operations on selected documents.
 */
public class DocumentCreateElapsedTimeModel extends AbstractDocumentChartModel {
	/**
	 * Executes getChartTitle.
	 * @param selectedDocument the selectedDocument value
	 * @return the result
	 */
	@Override
	protected String getChartTitle(String selectedDocument) {
		return "Create Request Elapsed Time - " + selectedDocument;
	}

	/**
	 * Executes getChartLabel.
	 * @return the result
	 */
	@Override
	protected String getChartLabel() {
		return "Elapsed Time (ms)";
	}

	/**
	 * Executes getChartColor.
	 * @return the result
	 */
	@Override
	protected Color getChartColor() {
		return Color.BLUE;
	}

	/**
	 * Executes getRequestKey.
	 * @param selectedDocument the selectedDocument value
	 * @return the result
	 */
	@Override
	protected String getRequestKey(String selectedDocument) {
		return "C" + selectedDocument;
	}

	/**
	 * Executes extractDataForTimePeriod.
	 * @param measurements the measurements value
	 * @param period the period value
	 * @return the result
	 */
	@Override
	protected Map<Integer, Integer> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractMillisForTimePeriod(measurements, period);
	}
}
