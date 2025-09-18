package modules.admin.MonitoringDashboard.models;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.Monitoring;
import org.skyve.util.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;

public class DocumentRAMDeltaModel extends ChartModel<MonitoringDashboard> {

	@SuppressWarnings("boxing")
	@Override
	public ChartData getChartData() {
		MonitoringDashboard bean = getBean();
		
		// Get the selected document name (e.g., "admin.Audit")
		String selectedDocument = getSelectedDocumentName(bean);
		
		ChartData cd = new ChartData();
		cd.setTitle("Create Request RAM Delta - " + selectedDocument);
		cd.setLabel("RAM Delta (%)");

		// Data structures for time series
		List<String> timeLabels = new ArrayList<>();
		List<Number> createValues = new ArrayList<>();

		// Get request measurements for create operations only
		String createKey = "C" + selectedDocument;
		
		RequestMeasurements createMeasurements = Monitoring.getRequestMeasurements(createKey);

		// Determine time period and get data
		Map<Integer, Number> createData = null;
		String timePeriod = "hours";
		
		if (createMeasurements != null) {
			createData = createMeasurements.getHoursRAMPercentageDelta();
			if (createData.isEmpty()) {
				createData = createMeasurements.getMinutesRAMPercentageDelta();
				timePeriod = "minutes";
				if (createData.isEmpty()) {
					createData = createMeasurements.getSecondsRAMPercentageDelta();
					timePeriod = "seconds";
				}
			}
		}

		// Build time series data
		buildTimeSeriesData(timeLabels, createValues, createData, timePeriod);
		
		// Update title based on actual time period
		cd.setTitle("Create Request RAM Delta - " + selectedDocument + " (" + getTimePeriodLabel(timePeriod) + ")");

		// Set up line chart data with create requests only
		cd.setLabels(timeLabels);
		cd.setValues(createValues);
		cd.setLabel("Create Requests");
		cd.setBackground(Color.ORANGE);
		cd.setBorder(Color.DARK_GRAY);

		// Debug info if no data
		if (createValues.isEmpty()) {
			cd.setTitle(cd.getTitle() + " - No create request data available");
		}

		return cd;
	}

	private String getSelectedDocumentName(MonitoringDashboard bean) {
		// Use the document name from the bean
		String documentName = bean.getDocumentName();
		return (documentName != null && !documentName.trim().isEmpty()) ? documentName : "admin.MonitoringDashboard";
	}

	private void buildTimeSeriesData(List<String> timeLabels, List<Number> createValues, 
									Map<Integer, Number> createData, String timePeriod) {
		
		// Determine the range of time indices to display
		int maxIndex = getMaxTimeIndex(timePeriod);
		
		for (int i = 0; i <= maxIndex; i++) {
			// Add time label
			timeLabels.add(formatTimeLabel(i, timePeriod));
			
			// Add create value (or 0 if no data)
			Number createValue = (createData != null) ? createData.get(i) : null;
			createValues.add((createValue != null) ? createValue : 0);
		}
		
		// If no data at all, add a placeholder
		if (timeLabels.isEmpty()) {
			timeLabels.add("No Data");
			createValues.add(0);
		}
	}

	private int getMaxTimeIndex(String timePeriod) {
		switch (timePeriod) {
			case "seconds":
				return 59;
			case "minutes":
				return 59;
			case "hours":
				return 23;
			case "days":
				return 6;
			case "weeks":
				return 51;
			default:
				return 23;
		}
	}

	private String formatTimeLabel(int index, String timePeriod) {
		switch (timePeriod) {
			case "seconds":
				return ":" + String.format("%02d", index);
			case "minutes":
				return String.format("%02d", index) + "m";
			case "hours":
				return String.format("%02d:00", index);
			case "days":
				return "Day " + (index + 1);
			case "weeks":
				return "Wk " + (index + 1);
			default:
				return String.valueOf(index);
		}
	}

	private String getTimePeriodLabel(String timePeriod) {
		switch (timePeriod) {
			case "seconds":
				return "Past 60 Seconds";
			case "minutes":
				return "Past 60 Minutes";
			case "hours":
				return "Past 24 Hours";
			case "days":
				return "Past 7 Days";
			case "weeks":
				return "Past 52 Weeks";
			default:
				return "Recent";
		}
	}
}