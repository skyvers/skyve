package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for request RAM usage monitoring.
 * Shows RAM usage deltas for the selected request type/module/document/component combination.
 */
public class RequestRAMUsageModel extends AbstractRequestChartModel {

	@Override
	protected String getChartTitle(String requestKey) {
		return "Request RAM Usage Delta - " + getRequestDescription(getBean());
	}

	@Override
	protected String getChartLabel() {
		return "RAM Usage Delta (%)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(46, 204, 113); // Green
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case pastMinute:
				return measurements.getSecondsRAMPercentageDelta();
			case pastHour:
				return measurements.getMinutesRAMPercentageDelta();
			case pastDay:
				return measurements.getHoursRAMPercentageDelta();
			case pastWeek:
				return measurements.getDaysRAMPercentageDelta();
			case pastYear:
				return measurements.getWeeksRAMPercentageDelta();
			default:
				return measurements.getHoursRAMPercentageDelta();
		}
	}
}