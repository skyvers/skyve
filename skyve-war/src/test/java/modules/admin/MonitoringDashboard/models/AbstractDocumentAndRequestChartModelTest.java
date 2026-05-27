package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;
import modules.test.AbstractSkyveTest;

/**
 * Tests for static utility methods in AbstractDocumentChartModel and AbstractRequestPeriodBarChartModel.
 */
@SuppressWarnings("static-method")
class AbstractDocumentAndRequestChartModelTest extends AbstractSkyveTest {

	/** Concrete impl for AbstractDocumentChartModel */
	private static class TestDocumentChartModel extends AbstractDocumentChartModel {
		@Override protected String getChartTitle(String selectedDocument) { return "Title:" + selectedDocument; }
		@Override protected String getChartLabel() { return "Label"; }
		@Override protected Color getChartColor() { return Color.BLUE; }
		@Override protected String getRequestKey(String selectedDocument) { return "E" + selectedDocument; }
		@Override protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) { return null; }

		// Expose protected methods for testing
		static String testGetSelectedDocumentName(MonitoringDashboard bean) {
			return getSelectedDocumentName(bean);
		}
		static void testBuildTimeSeriesData(List<String> labels, List<Number> values,
				Map<Integer, ? extends Number> data, Period p) {
			buildTimeSeriesData(labels, values, data, p);
		}
	}

	/** Concrete impl for AbstractRequestPeriodBarChartModel */
	private static class TestBarChartModel extends AbstractRequestPeriodBarChartModel {
		@Override protected String getChartTitle(String requestDescription) { return "Title"; }
		@Override protected String getChartLabel() { return "Label"; }
		@Override protected Color getChartColour() { return Color.RED; }
		@Override protected Map<Integer, ? extends Number> extractDataForTimePeriod(RequestMeasurements measurements, Period period) { return null; }

		static boolean testIsSignificantValue(Number value) {
			return isSignificantValue(value);
		}
		static boolean testIsDataValidForPeriod(RequestMeasurements m, Period p) {
			return isDataValidForPeriod(m, p);
		}
	}

	// ---- AbstractDocumentChartModel.getSelectedDocumentName ----

	@Test
	void getSelectedDocumentNameReturnsDocumentNameWhenSet() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setDocumentName("Audit");
		assertEquals("Audit", TestDocumentChartModel.testGetSelectedDocumentName(bean));
	}

	@Test
	void getSelectedDocumentNameReturnsAllDocumentsWhenNull() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		// documentName is null by default
		assertEquals("All Documents", TestDocumentChartModel.testGetSelectedDocumentName(bean));
	}

	@Test
	void getSelectedDocumentNameReturnsAllDocumentsWhenEmpty() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setDocumentName("   "); // whitespace only
		assertEquals("All Documents", TestDocumentChartModel.testGetSelectedDocumentName(bean));
	}

	// ---- AbstractDocumentChartModel.buildTimeSeriesData ----

	@Test
	void documentBuildTimeSeriesDataWithNullDataAddsPlaceholder() {
		List<String> labels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		TestDocumentChartModel.testBuildTimeSeriesData(labels, values, null, Period.currentMinute);
		assertEquals(1, labels.size());
		assertEquals("No Data", labels.get(0));
	}

	@Test
	void documentBuildTimeSeriesDataWithNonZeroValueAddsEntry() {
		List<String> labels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		Map<Integer, Integer> data = new HashMap<>();
		data.put(Integer.valueOf(15), Integer.valueOf(250));
		TestDocumentChartModel.testBuildTimeSeriesData(labels, values, data, Period.currentHour);
		assertEquals(1, labels.size());
		assertNotNull(labels.get(0));
		assertEquals(250, values.get(0).intValue());
	}

	@Test
	void documentBuildTimeSeriesDataWithZeroValueSkipsEntry() {
		List<String> labels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		Map<Integer, Integer> data = new HashMap<>();
		data.put(Integer.valueOf(0), Integer.valueOf(0));
		TestDocumentChartModel.testBuildTimeSeriesData(labels, values, data, Period.currentDay);
		assertEquals(1, labels.size());
		assertEquals("No Data", labels.get(0));
	}

	// ---- AbstractRequestPeriodBarChartModel.isSignificantValue ----

	@Test
	void isSignificantValueReturnsFalseForNull() {
		assertFalse(TestBarChartModel.testIsSignificantValue(null));
	}

	@Test
	void isSignificantValueReturnsFalseForZero() {
		assertFalse(TestBarChartModel.testIsSignificantValue(Integer.valueOf(0)));
	}

	@Test
	void isSignificantValueReturnsFalseForZeroDouble() {
		assertFalse(TestBarChartModel.testIsSignificantValue(Double.valueOf(0.0)));
	}

	@Test
	void isSignificantValueReturnsTrueForPositive() {
		assertTrue(TestBarChartModel.testIsSignificantValue(Integer.valueOf(100)));
	}

	@Test
	void isSignificantValueReturnsTrueForNegative() {
		assertTrue(TestBarChartModel.testIsSignificantValue(Integer.valueOf(-1)));
	}

	// ---- AbstractRequestPeriodBarChartModel.isDataValidForPeriod ----

	@Test
	void isDataValidForPeriodReturnsFalseForNull() {
		assertFalse(TestBarChartModel.testIsDataValidForPeriod(null, Period.currentMinute));
	}

	@Test
	void isDataValidForPeriodReturnsTrueWhenJustCreated() {
		RequestMeasurements measurements = new RequestMeasurements();
		// timeLastUpdate is initialized to System.currentTimeMillis()
		assertTrue(TestBarChartModel.testIsDataValidForPeriod(measurements, Period.currentMinute));
	}

	@Test
	void isDataValidForPeriodForAllPeriods() {
		RequestMeasurements measurements = new RequestMeasurements();
		measurements.updateMeasurements(50, (short) 50, (short) 50, (short) 50);
		// All periods should be valid immediately after recording
		assertTrue(TestBarChartModel.testIsDataValidForPeriod(measurements, Period.currentMinute));
		assertTrue(TestBarChartModel.testIsDataValidForPeriod(measurements, Period.currentHour));
		assertTrue(TestBarChartModel.testIsDataValidForPeriod(measurements, Period.currentDay));
		assertTrue(TestBarChartModel.testIsDataValidForPeriod(measurements, Period.currentWeek));
		assertTrue(TestBarChartModel.testIsDataValidForPeriod(measurements, Period.currentYear));
	}
}
