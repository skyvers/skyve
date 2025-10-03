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

public class RequestTypeComparisonModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();

		// Default selections if not set
		String requestType = (bean.getRequestType() != null) ? bean.getRequestType().toString() : "edit";
		String metric = (bean.getMetric() != null) ? bean.getMetric().toString() : "elapsedTime";
		String period = (bean.getPeriod() != null) ? bean.getPeriod().toString() : "oneHour";
		Integer topCount = (bean.getTopN() != null) ? bean.getTopN() : 10; // Default to top 10

		ChartData cd = new ChartData();
		cd.setLabel(getMetricLabel(metric));
		cd.setTitle(getChartTitle(requestType, metric, period, topCount));

		// Get all request key codes from monitoring
		Set<String> requestKeyCodes = Monitoring.getRequestKeyCodes();

		// Data structures to hold our chart data
		List<String> documentLabels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		List<Color> backgrounds = new ArrayList<>();
		List<Color> borders = new ArrayList<>();

		// Filter for the selected request type and collect data
		String requestPrefix = getRequestPrefix(requestType);
		int totalKeysChecked = 0;
		int matchingKeys = 0;
		int keysWithData = 0;

		for (String keyCode : requestKeyCodes) {
			totalKeysChecked++;

			// Handle 'all' request type case
			boolean includeKey = false;
			if (requestPrefix == null) {
				// 'all' case - include all request types (C, E, U, Q, M, G, L, P, R, A, N, H, O, S, Z, T, D, J, B, W, X, V
				// prefixes)
				includeKey = keyCode.startsWith("C") || keyCode.startsWith("E") || keyCode.startsWith("U") ||
						keyCode.startsWith("Q") || keyCode.startsWith("M") || keyCode.startsWith("G") ||
						keyCode.startsWith("L") || keyCode.startsWith("P") || keyCode.startsWith("R") ||
						keyCode.startsWith("A") || keyCode.startsWith("N") || keyCode.startsWith("H") ||
						keyCode.startsWith("O") || keyCode.startsWith("S") || keyCode.startsWith("Z") ||
						keyCode.startsWith("T") || keyCode.startsWith("D") || keyCode.startsWith("J") ||
						keyCode.startsWith("B") || keyCode.startsWith("W") || keyCode.startsWith("V");
			} else {
				// Specific request type case
				includeKey = keyCode.startsWith(requestPrefix);
			}

			if (includeKey) {
				matchingKeys++;
				RequestMeasurements measurements = Monitoring.getRequestMeasurements(keyCode);
				if (measurements != null) {
					// Get the appropriate data based on selected period and metric
					Map<Integer, ? extends Number> data = getData(measurements, period, metric);

					// Calculate average value for this document
					if (!data.isEmpty()) {
						keysWithData++;
						double total = data.values().stream().mapToDouble(Number::doubleValue).sum();
						double avg = total / data.size();

						// Extract document name from keyCode
						String documentName = extractDocumentName(keyCode, requestType);

						// For RAM and CPU metrics, allow negative values (they represent changes)
						// Only filter out zero values for elapsed time
						boolean includeValue = false;
						if ("elapsedTime".equals(metric)) {
							includeValue = avg > 0;
						} else {
							includeValue = avg != 0; // Include negative values for CPU/RAM deltas
						}

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

		// If no data found, add a placeholder to show debugging info
		if (documentLabels.isEmpty()) {
			documentLabels.add("No Data Available");
			values.add(0);
			backgrounds.add(Color.LIGHT_GRAY);
			borders.add(Color.GRAY);

			// Update title to show debug info
			String prefixDesc = (requestPrefix == null) ? "all types (C/E/U/Q/M/G/L/P/R/A/N/H/O/S/Z/T/D/J/B/W/X/V)"
					: "prefix '" + requestPrefix + "'";
			cd.setTitle(getChartTitle(requestType, metric, period, topCount) +
					" [Debug: " + totalKeysChecked + " total keys, " +
					matchingKeys + " matching " + prefixDesc + ", " +
					keysWithData + " with data]");
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
	private String extractDocumentName(String keyCode, String requestType) {
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
			if ("all".equals(requestType) && documentName != null) {
				String typeLabel = getRequestTypeLabel(keyCode.substring(0, 1));
				return typeLabel + ": " + documentName;
			}

			return documentName;
		}
		return null;
	}

	/**
	 * Map request type enum to request key prefix
	 */
	private static String getRequestPrefix(String requestType) {
		switch (requestType) {
			case "all":
				return null; // Special case - we'll handle this differently
			case "C":
				return "C";
			case "E":
				return "E";
			case "U":
				return "U";
			case "Q":
				return "Q";
			case "M":
				return "M";
			case "G":
				return "G";
			case "L":
				return "L";
			case "P":
				return "P";
			case "R":
				return "R";
			case "A":
				return "A";
			case "N":
				return "N";
			case "H":
				return "H";
			case "O":
				return "O";
			case "S":
				return "S";
			case "Z":
				return "Z";
			case "T":
				return "T";
			case "D":
				return "D";
			case "J":
				return "J";
			case "B":
				return "B";
			case "W":
				return "W";
			case "V":
				return "V";
			default:
				return "E"; // Default to edit
		}
	}

	/**
	 * Get metric label for chart
	 */
	private static String getMetricLabel(String metric) {
		switch (metric) {
			case "elapsedTime":
				return "Elapsed Time (ms)";
			case "CPUTimeDelta":
				return "CPU Time";
			case "RAMUsageDelta":
				return "RAM Usage Delta (%)";
			default:
				return "Value";
		}
	}

	/**
	 * Generate chart title based on selections
	 */
	private String getChartTitle(String requestType, String metric, String period, Integer topCount) {
		String typeLabel = getRequestTypeLabel(requestType);
		String metricLabel = getShortMetricLabel(metric);
		String periodLabel = getPeriodLabel(period);
		String units = getMetricUnits(metric);

		return "Top " + topCount + " " + typeLabel + " " + metricLabel + " (" + units + ") - " + periodLabel;
	}

	private String getRequestTypeLabel(String requestType) {
		switch (requestType) {
			case "all":
				return "All Requests";
			case "C":
				return "Create";
			case "E":
				return "Edit";
			case "U":
				return "SmartEdit";
			case "Q":
				return "Query/List";
			case "M":
				return "Model";
			case "G":
				return "Generate";
			case "L":
				return "Legacy List";
			case "P":
				return "Map";
			case "R":
				return "Content";
			case "A":
				return "AJAX";
			case "N":
				return "Page";
			case "H":
				return "Model Ops";
			case "O":
				return "Attribute";
			case "S":
				return "Search";
			case "Z":
				return "Snap";
			case "T":
				return "Tag";
			case "D":
				return "Dynamic Image";
			case "J":
				return "Report";
			case "B":
				return "Bizport Export";
			case "W":
				return "Download";
			case "V":
				return "Customer Resource";
			default:
				return "Request";
		}
	}

	private String getShortMetricLabel(String metric) {
		switch (metric) {
			case "elapsedTime":
				return "Performance";
			case "CPUTimeDelta":
				return "CPU Time";
			case "RAMUsageDelta":
				return "RAM Impact";
			default:
				return "Metrics";
		}
	}

	private String getPeriodLabel(String period) {
		switch (period) {
			case "oneMinute":
				return "Past Minute";
			case "oneHour":
				return "Past Hour";
			case "oneDay":
				return "Past 24 Hours";
			case "oneWeek":
				return "Past Week";
			case "oneYear":
				return "Past Year";
			default:
				return "Recent";
		}
	}

	/**
	 * Get the units for a metric
	 */
	private String getMetricUnits(String metric) {
		switch (metric) {
			case "elapsedTime":
				return "ms";
			case "CPUTimeDelta":
				return "ms";
			case "RAMUsageDelta":
				return "%";
			default:
				return "";
		}
	}

	/**
	 * Get data from RequestMeasurements based on period and metric
	 */
	private Map<Integer, ? extends Number> getData(RequestMeasurements measurements, String period, String metric) {
		// First determine the time period data source
		Map<Integer, ? extends Number> timeData;
		switch (period) {
			case "oneMinute":
				timeData = getMetricData(measurements, "seconds", metric);
				break;
			case "oneHour":
				timeData = getMetricData(measurements, "minutes", metric);
				break;
			case "oneDay":
				timeData = getMetricData(measurements, "hours", metric);
				break;
			case "oneWeek":
				timeData = getMetricData(measurements, "days", metric);
				break;
			case "oneYear":
				timeData = getMetricData(measurements, "weeks", metric);
				break;
			default:
				timeData = getMetricData(measurements, "hours", metric);
		}

		// Fallback to shorter periods if no data available
		if (timeData.isEmpty() && !period.equals("oneMinute")) {
			timeData = getMetricData(measurements, "minutes", metric);
		}
		if (timeData.isEmpty()) {
			timeData = getMetricData(measurements, "seconds", metric);
		}

		return timeData;
	}

	/**
	 * Get specific metric data for a time period
	 */
	private Map<Integer, ? extends Number> getMetricData(RequestMeasurements measurements, String timePeriod, String metric) {
		switch (metric) {
			case "elapsedTime":
				return getElapsedTimeData(measurements, timePeriod);
			case "CPUTimeDelta":
				return getCPUData(measurements, timePeriod);
			case "RAMUsageDelta":
				return getRAMData(measurements, timePeriod);
			default:
				return getElapsedTimeData(measurements, timePeriod);
		}
	}

	private Map<Integer, Integer> getElapsedTimeData(RequestMeasurements measurements, String timePeriod) {
		switch (timePeriod) {
			case "seconds":
				return measurements.getSecondsMillis();
			case "minutes":
				return measurements.getMinutesMillis();
			case "hours":
				return measurements.getHoursMillis();
			case "days":
				return measurements.getDaysMillis();
			case "weeks":
				return measurements.getWeeksMillis();
			default:
				return measurements.getHoursMillis();
		}
	}

	private Map<Integer, Integer> getCPUData(RequestMeasurements measurements, String timePeriod) {
		switch (timePeriod) {
			case "seconds":
				return measurements.getSecondsCPUTimeDelta();
			case "minutes":
				return measurements.getMinutesCPUTimeDelta();
			case "hours":
				return measurements.getHoursCPUTimeDelta();
			case "days":
				return measurements.getDaysCPUTimeDelta();
			case "weeks":
				return measurements.getWeeksCPUTimeDelta();
			default:
				return measurements.getHoursCPUTimeDelta();
		}
	}

	private Map<Integer, Float> getRAMData(RequestMeasurements measurements, String timePeriod) {
		switch (timePeriod) {
			case "seconds":
				return measurements.getSecondsRAMPercentageDelta();
			case "minutes":
				return measurements.getMinutesRAMPercentageDelta();
			case "hours":
				return measurements.getHoursRAMPercentageDelta();
			case "days":
				return measurements.getDaysRAMPercentageDelta();
			case "weeks":
				return measurements.getWeeksRAMPercentageDelta();
			default:
				return measurements.getHoursRAMPercentageDelta();
		}
	}

}
