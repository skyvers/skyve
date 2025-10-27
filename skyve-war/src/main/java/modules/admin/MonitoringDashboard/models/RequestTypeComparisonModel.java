package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.Monitoring;
import org.skyve.util.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Metric;
import modules.admin.domain.MonitoringDashboard.Period;
import modules.admin.domain.MonitoringDashboard.RequestType;

public class RequestTypeComparisonModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
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
		Integer topCount = (bean.getTopN() != null) ? bean.getTopN() : 10; // Default to top 10

		// If required selections are null show chart has no data
		if (requestType == null || metric == null || period == null) {
			documentLabels.add("No Data Available");
			values.add(0);
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
		@SuppressWarnings("unused")
		int totalKeysChecked = 0;
		@SuppressWarnings("unused")
		int matchingKeys = 0;
		@SuppressWarnings("unused")
		int keysWithData = 0;

		for (String keyCode : requestKeyCodes) {
			totalKeysChecked++;

			// Handle 'all' request type case
			boolean includeKey = false;
			if (requestPrefix == null) {
				// 'all' case - include all request types
				includeKey = keyCode.startsWith("C") || keyCode.startsWith("E") || keyCode.startsWith("U")
						|| keyCode.startsWith("Q") || keyCode.startsWith("M") || keyCode.startsWith("L") || keyCode.startsWith("P")
						|| keyCode.startsWith("H") || keyCode.startsWith("O") || keyCode.startsWith("J") || keyCode.startsWith("B")
						|| keyCode.startsWith("W");
			} else {
				// Specific request type case
				includeKey = keyCode.startsWith(requestPrefix);
			}

			if (includeKey) {
				matchingKeys++;
				RequestMeasurements measurements = Monitoring.getRequestMeasurements(keyCode);
				if (measurements != null) {
					// Get the appropriate data based on selected period and metric
					Map<Integer, ? extends Number> data = getMetricData(measurements, period, metric);

					// Calculate average value for this document
					if (!data.isEmpty()) {
						keysWithData++;
						double total = data.values().stream().mapToDouble(Number::doubleValue).sum();
						double avg = total / data.size();

						// Extract document name from keyCode
						String documentName = extractDocumentName(keyCode, requestType);

						// Only show values gretaer than 0
						boolean includeValue = false;
						includeValue = avg > 0;

						if (includeValue && documentName != null) {
							documentLabels.add(documentName);
							values.add(avg);

							// Add some color variety
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
			values.add(0);
			backgrounds.add(Color.LIGHT_GRAY);
			borders.add(Color.GRAY);
			cd.setTitle(getChartTitle(requestType, metric, period, topCount) +
					" [No Data]");
			return cd;
		}

		// Sort by values (descending - highest first)
		List<Integer> indices = new ArrayList<>();
		for (int i = 0; i < values.size(); i++) {
			indices.add(i);
		}

		indices.sort((i, j) -> Double.compare(values.get(j).doubleValue(), values.get(i).doubleValue()));

		// Limit to top N results
		int limitCount = Math.min(topCount, indices.size());
		List<Integer> topIndices = indices.subList(0, limitCount);

		// Create sorted and limited lists
		List<String> sortedLabels = topIndices.stream().map(documentLabels::get).collect(Collectors.toList());
		List<Number> sortedValues = topIndices.stream().map(values::get).collect(Collectors.toList());
		List<Color> sortedBackgrounds = topIndices.stream().map(backgrounds::get).collect(Collectors.toList());
		List<Color> sortedBorders = topIndices.stream().map(borders::get).collect(Collectors.toList());

		// Set the chart data
		cd.setLabels(sortedLabels);
		cd.setValues(sortedValues);
		cd.setBackgrounds(sortedBackgrounds);
		cd.setBorders(sortedBorders);

		return cd;
	}

	/**
	 * Extract document name from request key code.
	 * Key format: <type><module>.<document> or <type><module>^<component>
	 */
	@SuppressWarnings("static-method")
	private String extractDocumentName(String keyCode, RequestType requestType) {
		if (keyCode.length() > 1) {
			String moduleDoc = keyCode.substring(1); // Remove type prefix
			int dotIndex = moduleDoc.indexOf('.');
			int caretIndex = moduleDoc.indexOf('^');

			String documentName = null;

			// Check if module is "null" - if so, use component directly
			if (caretIndex > 0) {
				String modulePart = moduleDoc.substring(0, caretIndex);
				String componentPart = moduleDoc.substring(caretIndex + 1);

				if ("null".equals(modulePart)) {
					// When module is null, use the component as the document name
					documentName = componentPart;
				} else if (dotIndex > 0 && dotIndex < caretIndex) {
					// Format: <type><module>.<document>^<component>
					String document = moduleDoc.substring(dotIndex + 1, caretIndex);
					documentName = document;
				} else {
					// Format: <type><module>^<component>
					documentName = modulePart + "^" + componentPart;
				}
			} else if (dotIndex > 0 && dotIndex < moduleDoc.length() - 1) {
				// Format: <type><module>.<document> (no component)
				String document = moduleDoc.substring(dotIndex + 1);
				documentName = document;
			}

			// For 'all' request type, include the request type prefix to distinguish
			if (requestType == RequestType.all && documentName != null) {
				String typeLabel = RequestType.fromCode(keyCode.substring(0, 1)).toLocalisedDescription();
				return typeLabel + ": " + documentName;
			}

			return documentName;
		}
		return null;
	}

	/**
	 * Get metric label for chart
	 */
	private static String getMetricLabel(Metric metric) {
		switch (metric) {
			case elapsedTime:
				return "Elapsed Time (ms)";
			case CPUTimeDelta:
				return "CPU Time";
			case RAMUsageDelta:
				return "RAM Usage Delta (%)";
			default:
				return "Value";
		}
	}

	/**
	 * Generate chart title based on selections
	 */
	private static String getChartTitle(RequestType requestType, Metric metric, Period period, Integer topCount) {
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
			case elapsedTime:
				return getElapsedTimeData(measurements, timePeriod);
			case CPUTimeDelta:
				return getCPUData(measurements, timePeriod);
			case RAMUsageDelta:
				return getRAMData(measurements, timePeriod);
			default:
				return getElapsedTimeData(measurements, timePeriod);
		}
	}

	private static Map<Integer, Integer> getElapsedTimeData(RequestMeasurements measurements, Period timePeriod) {
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

	private static Map<Integer, Integer> getCPUData(RequestMeasurements measurements, Period timePeriod) {
		switch (timePeriod) {
			case currentMinute:
				return measurements.getSecondsCPUTimeDelta();
			case currentHour:
				return measurements.getMinutesCPUTimeDelta();
			case currentDay:
				return measurements.getHoursCPUTimeDelta();
			case currentWeek:
				return measurements.getDaysCPUTimeDelta();
			case currentYear:
				return measurements.getWeeksCPUTimeDelta();
			default:
				return measurements.getHoursCPUTimeDelta();
		}
	}

	private static Map<Integer, Float> getRAMData(RequestMeasurements measurements, Period timePeriod) {
		switch (timePeriod) {
			case currentMinute:
				return measurements.getSecondsRAMPercentageDelta();
			case currentHour:
				return measurements.getMinutesRAMPercentageDelta();
			case currentDay:
				return measurements.getHoursRAMPercentageDelta();
			case currentWeek:
				return measurements.getDaysRAMPercentageDelta();
			case currentYear:
				return measurements.getWeeksRAMPercentageDelta();
			default:
				return measurements.getHoursRAMPercentageDelta();
		}
	}

}
