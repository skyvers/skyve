package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.Map;

import org.skyve.util.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Chart model for request CPU time monitoring.
 * Shows CPU time deltas for the selected request type/module/document/component combination.
 */
public class RequestCPUTimeModel extends AbstractRequestChartModel {

	@Override
	protected String getChartTitle(String requestKey) {
		return "Request CPU Time Delta - " + getRequestDescription(getBean());
	}

	@Override
	protected String getChartLabel() {
		return "CPU Time Delta (ms)";
	}

	@Override
	protected Color getChartColor() {
		return new Color(231, 76, 60); // Red
	}

	@Override
	protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) {
		switch (period) {
			case pastMinute:
				return measurements.getSecondsCPUTimeDelta();
			case pastHour:
				return measurements.getMinutesCPUTimeDelta();
			case pastDay:
				return measurements.getHoursCPUTimeDelta();
			case pastWeek:
				return measurements.getDaysCPUTimeDelta();
			case pastYear:
				return measurements.getWeeksCPUTimeDelta();
			default:
				return measurements.getHoursCPUTimeDelta();
		}
	}
}