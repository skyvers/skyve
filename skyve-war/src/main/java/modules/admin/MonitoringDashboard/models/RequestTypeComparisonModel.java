package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.monitoring.Monitoring;
import org.skyve.util.monitoring.RequestKey;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Metric;
import modules.admin.domain.MonitoringDashboard.Period;
import modules.admin.domain.MonitoringDashboard.RequestType;

public class RequestTypeComparisonModel extends ChartModel<MonitoringDashboard> {

	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();

		// Data structures to hold our chart data
		ChartData cd = new ChartData();
		List<String> documentLabels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		List<Color> backgrounds = new ArrayList<>();
		List<Color> borders = new ArrayList<>();

		// Get user selections
		RequestType requestType = bean.getRequestType();
		Metric metric = bean.getMetric();
		Period period = bean.getPeriod();
		int topCount = (bean.getTopN() != null) ? bean.getTopN().intValue() : 10; // Default to top 10

		// If required selections are null show chart has no data
		if (requestType == null || metric == null || period == null) {
			documentLabels.add("No Data Available");
			values.add(Integer.valueOf(0));
			backgrounds.add(Color.LIGHT_GRAY);
			borders.add(Color.GRAY);
			cd.setTitle("No Data");
			return cd;
		}

		cd.setLabel(getMetricLabel(metric));
		cd.setTitle(getChartTitle(requestType, metric, period, topCount));

		// Get all request key codes from monitoring
		Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();

		// Filter for the selected request type and collect data
		String requestPrefix = requestType != RequestType.all ? requestType.toCode() : null;

		for (String keyCode : requestKeyCodes) {
			// Handle 'all' request type case, or a specific request prefix
			if ((requestPrefix == null) || keyCode.startsWith(requestPrefix)) {
				RequestMeasurements measurements = Monitoring.getRequestMeasurements(keyCode);
				if (measurements != null) {
					// Get the appropriate data based on selected period and metric
					Map<Integer, ? extends Number> data = getMetricData(measurements, period, metric);

					// Calculate average value for this document
					if (! data.isEmpty()) {
						double total = data.values().stream().mapToDouble(Number::doubleValue).sum();
						double avg = total / data.size();

						// Extract document name from keyCode
						String documentName = RequestKey.fromString(keyCode).toDescription();

						// Only show values greater than 0
						boolean includeValue = false;
						includeValue = avg > 0;

						if (includeValue && documentName != null) {
							documentLabels.add(documentName);
							values.add(Double.valueOf(avg));

							// Add some colour variety
							float hue = (documentLabels.size() * 0.618034f) % 1.0f; // Golden ratio for nice color distribution
							Color backgroundColor = Color.getHSBColor(hue, 0.7f, 0.9f);
							Color borderColor = Color.getHSBColor(hue, 0.9f, 0.7f);

							backgrounds.add(backgroundColor);
							borders.add(borderColor);
						}
					}
				}
			}
		}

		// If no data found, show a no-data chart
		if (documentLabels.isEmpty()) {
			documentLabels.add("No Data Available");
			values.add(Integer.valueOf(0));
			backgrounds.add(Color.LIGHT_GRAY);
			borders.add(Color.GRAY);
			cd.setTitle(getChartTitle(requestType, metric, period, topCount) +
					" [No Data]");
			return cd;
		}

		// Sort by values (descending - highest first)
		List<Integer> indices = new ArrayList<>();
		for (int i = 0; i < values.size(); i++) {
			indices.add(Integer.valueOf(i));
		}

		indices.sort((i, j) -> Double.compare(values.get(j.intValue()).doubleValue(), values.get(i.intValue()).doubleValue()));

		// Limit to top N results
		int limitCount = Math.min(topCount, indices.size());
		List<Integer> topIndices = indices.subList(0, limitCount);

		// Create sorted and limited lists
		List<String> sortedLabels = topIndices.stream().map(documentLabels::get).toList();
		List<Number> sortedValues = topIndices.stream().map(values::get).toList();
		List<Color> sortedBackgrounds = topIndices.stream().map(backgrounds::get).toList();
		List<Color> sortedBorders = topIndices.stream().map(borders::get).toList();

		// Set the chart data
		cd.setLabels(sortedLabels);
		cd.setValues(sortedValues);
		cd.setBackgrounds(sortedBackgrounds);
		cd.setBorders(sortedBorders);

		return cd;
	}

	/**
	 * Get metric label for chart
	 */
	private static String getMetricLabel(Metric metric) {
		switch (metric) {
			case elapsedRequestTime:
				return "Elapsed Requst Time (ms)";
			case requestCPUUtilisation:
				return "Request CPU Utilisation (%)";
			case systemCPUUsage:
				return "System CPU Usage (%)";
			case systemRAMUsage:
				return "Heap RAM Usage (%)";
			default:
				throw new IllegalStateException("unhandled metric " + metric);
		}
	}

	/**
	 * Generate chart title based on selections
	 */
	private static String getChartTitle(RequestType requestType, Metric metric, Period period, int topCount) {
		String typeLabel = requestType.toLocalisedDescription();
		String metricLabel = getMetricLabel(metric);
		String periodLabel = period != null ? period.toLocalisedDescription() : "Recent";

		return "Top " + topCount + " " + typeLabel + " " + metricLabel + " - " + periodLabel;
	}

	/**
	 * Get specific metric data for a time period
	 */
	private static Map<Integer, ? extends Number> getMetricData(RequestMeasurements measurements, Period timePeriod, Metric metric) {
		switch (metric) {
			case elapsedRequestTime:
				return getElapsedTime(measurements, timePeriod);
			case requestCPUUtilisation:
				return getCpuUtilisation(measurements, timePeriod);
			case systemCPUUsage:
				return getCpuUsage(measurements, timePeriod);
			case systemRAMUsage:
				return getRamUsage(measurements, timePeriod);
			default:
				return getElapsedTime(measurements, timePeriod);
		}
	}

	private static Map<Integer, Integer> getElapsedTime(RequestMeasurements measurements, Period timePeriod) {
		switch (timePeriod) {
			case currentMinute:
				return measurements.getSecondsMillis();
			case currentHour:
				return measurements.getMinutesMillis();
			case currentDay:
				return measurements.getHoursMillis();
			case currentWeek:
				return measurements.getDaysMillis();
			case currentYear:
				return measurements.getWeeksMillis();
			default:
				return measurements.getHoursMillis();
		}
	}

	private static Map<Integer, Float> getCpuUtilisation(RequestMeasurements measurements, Period timePeriod) {
		switch (timePeriod) {
			case currentMinute:
				return measurements.getSecondsCpuUtilisation();
			case currentHour:
				return measurements.getMinutesCpuUtilisation();
			case currentDay:
				return measurements.getHoursCpuUtilisation();
			case currentWeek:
				return measurements.getDaysCpuUtilisation();
			case currentYear:
				return measurements.getWeeksCpuUtilisation();
			default:
				return measurements.getHoursCpuUtilisation();
		}
	}

	private static Map<Integer, Float> getCpuUsage(RequestMeasurements measurements, Period timePeriod) {
		switch (timePeriod) {
			case currentMinute:
				return measurements.getSecondsSystemCpuUsage();
			case currentHour:
				return measurements.getMinutesSystemCpuUsage();
			case currentDay:
				return measurements.getHoursSystemCpuUsage();
			case currentWeek:
				return measurements.getDaysSystemCpuUsage();
			case currentYear:
				return measurements.getWeeksSystemCpuUsage();
			default:
				return measurements.getHoursSystemCpuUsage();
		}
	}

	private static Map<Integer, Float> getRamUsage(RequestMeasurements measurements, Period timePeriod) {
		switch (timePeriod) {
			case currentMinute:
				return measurements.getSecondsHeapRamUsage();
			case currentHour:
				return measurements.getMinutesHeapRamUsage();
			case currentDay:
				return measurements.getHoursHeapRamUsage();
			case currentWeek:
				return measurements.getDaysHeapRamUsage();
			case currentYear:
				return measurements.getWeeksHeapRamUsage();
			default:
				return measurements.getHoursHeapRamUsage();
		}
	}
}
