package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.util.monitoring.RequestMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;
import modules.admin.domain.MonitoringDashboard.RequestType;
import modules.test.AbstractSkyveTest;

/**
 * Tests for the static utility methods in AbstractMonitoringChartModel.
 * Uses a minimal concrete subclass since the methods are protected.
 */
@SuppressWarnings("static-method")
class AbstractMonitoringChartModelTest extends AbstractSkyveTest {

	/** Minimal concrete subclass to expose protected static methods. */
	private static class TestChartModel extends AbstractMonitoringChartModel {
		@Override
		public ChartData getChartData() {
			return null;
		}

		// Bridge methods to test the protected statics
		static boolean testIsDataValid(RequestMeasurements m, Period p) {
			return isDataValidForCurrentPeriod(m, p);
		}

		static String testGetTimePeriodLabel(Period p) {
			return getTimePeriodLabel(p);
		}

		static void testBuildTimeSeriesData(List<String> labels, List<Number> values,
				Map<Integer, ? extends Number> data, Period p) {
			buildTimeSeriesData(labels, values, data, p);
		}

		static String testBuildRequestKey(MonitoringDashboard bean) {
			return buildRequestKey(bean);
		}

		static String testGetRequestDescription(MonitoringDashboard bean) {
			return getRequestDescription(bean);
		}
	}

	// ---- isDataValidForCurrentPeriod ----

	@Test
	void isDataValidForCurrentPeriodReturnsFalseWhenNull() {
		assertFalse(TestChartModel.testIsDataValid(null, Period.currentMinute));
	}

	@Test
	void isDataValidForCurrentPeriodReturnsTrueWhenJustCreated() {
		RequestMeasurements measurements = new RequestMeasurements();
		// timeLastUpdate is initialized to System.currentTimeMillis(), so it's valid
		assertTrue(TestChartModel.testIsDataValid(measurements, Period.currentMinute));
	}

	@Test
	void isDataValidForCurrentPeriodReturnsTrueWhenRecentlyUpdated() {
		RequestMeasurements measurements = new RequestMeasurements();
		// Record a request so the last update time gets set
		measurements.updateMeasurements(50, (short) 50, (short) 50, (short) 50);
		// Should be valid for currentMinute (updated less than 2 minutes ago)
		assertTrue(TestChartModel.testIsDataValid(measurements, Period.currentMinute));
	}

	// ---- getTimePeriodLabel ----

	@Test
	void getTimePeriodLabelCurrentMinute() {
		assertEquals("Current Minute", TestChartModel.testGetTimePeriodLabel(Period.currentMinute));
	}

	@Test
	void getTimePeriodLabelCurrentHour() {
		assertEquals("Current Hour", TestChartModel.testGetTimePeriodLabel(Period.currentHour));
	}

	@Test
	void getTimePeriodLabelCurrentDay() {
		assertEquals("Current Day", TestChartModel.testGetTimePeriodLabel(Period.currentDay));
	}

	@Test
	void getTimePeriodLabelCurrentWeek() {
		assertEquals("Current Week", TestChartModel.testGetTimePeriodLabel(Period.currentWeek));
	}

	@Test
	void getTimePeriodLabelCurrentYear() {
		assertEquals("Current Year", TestChartModel.testGetTimePeriodLabel(Period.currentYear));
	}

	// ---- buildTimeSeriesData ----

	@Test
	void buildTimeSeriesDataWithNullDataAddsPlaceholder() {
		List<String> labels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		TestChartModel.testBuildTimeSeriesData(labels, values, null, Period.currentMinute);
		assertEquals(1, labels.size());
		assertEquals("No Data", labels.get(0));
		assertEquals(1, values.size());
		assertEquals(0, values.get(0).intValue());
	}

	@Test
	void buildTimeSeriesDataWithEmptyDataAddsPlaceholder() {
		List<String> labels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		Map<Integer, Integer> data = new HashMap<>();
		TestChartModel.testBuildTimeSeriesData(labels, values, data, Period.currentMinute);
		assertEquals(1, labels.size());
		assertEquals("No Data", labels.get(0));
	}

	@Test
	void buildTimeSeriesDataWithZeroValueSkipsEntry() {
		List<String> labels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		Map<Integer, Integer> data = new HashMap<>();
		data.put(Integer.valueOf(0), Integer.valueOf(0));
		TestChartModel.testBuildTimeSeriesData(labels, values, data, Period.currentMinute);
		// Zero values are skipped, so placeholder added
		assertEquals(1, labels.size());
		assertEquals("No Data", labels.get(0));
	}

	@Test
	void buildTimeSeriesDataWithNonZeroValueAddsEntry() {
		List<String> labels = new ArrayList<>();
		List<Number> values = new ArrayList<>();
		Map<Integer, Integer> data = new HashMap<>();
		data.put(Integer.valueOf(30), Integer.valueOf(100));
		TestChartModel.testBuildTimeSeriesData(labels, values, data, Period.currentMinute);
		assertEquals(1, labels.size());
		assertNotNull(labels.get(0));
		assertEquals(1, values.size());
		assertEquals(100, values.get(0).intValue());
	}

	// ---- buildRequestKey ----

	@Test
	void buildRequestKeyWithNoSelections() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		String key = TestChartModel.testBuildRequestKey(bean);
		assertNotNull(key);
		// Should start with the default request type
		assertTrue(key.startsWith(RequestType.E.toCode()), "Key should start with default type E, got: " + key);
	}

	@Test
	void buildRequestKeyWithModuleName() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setRsModuleName("admin");
		String key = TestChartModel.testBuildRequestKey(bean);
		assertTrue(key.contains("admin"), "Key should contain module name");
	}

	@Test
	void buildRequestKeyWithModuleAndDocument() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setRsModuleName("admin");
		bean.setRsDocumentName("User");
		String key = TestChartModel.testBuildRequestKey(bean);
		assertTrue(key.contains("admin.User"), "Key should contain module.document");
	}

	@Test
	void buildRequestKeyWithComponent() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setRsModuleName("admin");
		bean.setRsDocumentName("User");
		bean.setRsComponentName("editButton");
		String key = TestChartModel.testBuildRequestKey(bean);
		assertTrue(key.contains("^editButton"), "Key should contain ^component");
	}

	@Test
	void buildRequestKeyWithExplicitRequestType() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setRsRequestType(RequestType.A);
		String key = TestChartModel.testBuildRequestKey(bean);
		assertTrue(key.startsWith(RequestType.A.toCode()), "Key should start with A");
	}

	// ---- getRequestDescription ----

	@Test
	void getRequestDescriptionWithNoSelections() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		String desc = TestChartModel.testGetRequestDescription(bean);
		assertNotNull(desc);
		assertFalse(desc.isEmpty());
	}

	@Test
	void getRequestDescriptionWithModuleName() {
		MonitoringDashboard bean = MonitoringDashboard.newInstance();
		bean.setRsModuleName("admin");
		String desc = TestChartModel.testGetRequestDescription(bean);
		assertTrue(desc.contains("admin"), "Description should contain module name");
	}
}
