package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Bar chart model for request Heap RAM usage period averages.
 * Shows average Heap RAM usages across different time periods (seconds, minutes, hours, days, weeks).
 */
public class RequestHeapRamUsagePeriodBarModel extends AbstractRequestPeriodBarChartModel {
	@Override
	protected String getChartTitle(String requestDescription) {
		return "Heap RAM Usage Averages - " + requestDescription;
	}

	@Override
	protected String getChartLabel() {
		return "Average Heap RAM Usage (%)";
	}

	@Override
	protected Color getChartColour() {
		return new Color(46, 204, 113); // Green
	}

	@Override
	protected Map<Integer, Float> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		return RequestListModel.extractHeapRamUsageForTimePeriod(measurements, period);
	}
}
