package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.view.model.chart.ChartData;

import org.skyve.util.monitoring.Monitoring;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Metric;
import modules.admin.domain.MonitoringDashboard.Period;
import modules.admin.domain.MonitoringDashboard.RequestType;

/**
 * Tests for RequestTypeComparisonModel.getChartData().
 */
@SuppressWarnings("static-method")
class RequestTypeComparisonModelTest {

	private RequestTypeComparisonModel model;
	private MonitoringDashboard bean;

	@BeforeEach
	void setUp() {
		Monitoring.purge(); // ensure no cross-test static state from MonitoringTest in skyve-core
		model = new RequestTypeComparisonModel();
		bean = new MonitoringDashboard();
		model.setBean(bean);
	}

	// --- null field guards ---

	@Test
	void testGetChartDataNullRequestTypeReturnsNoDataChart() {
		bean.setRequestType(null);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentHour);

		ChartData cd = model.getChartData();

		assertNotNull(cd);
		assertEquals("No Data", cd.getTitle());
		assertHasNoDataLabel(cd);
	}

	@Test
	void testGetChartDataNullMetricReturnsNoDataChart() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(null);
		bean.setPeriod(Period.currentHour);

		ChartData cd = model.getChartData();

		assertNotNull(cd);
		assertEquals("No Data", cd.getTitle());
		assertHasNoDataLabel(cd);
	}

	@Test
	void testGetChartDataNullPeriodReturnsNoDataChart() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(null);

		ChartData cd = model.getChartData();

		assertNotNull(cd);
		assertEquals("No Data", cd.getTitle());
		assertHasNoDataLabel(cd);
	}

	@Test
	void testGetChartDataAllNullReturnsNoDataChart() {
		// MonitoringDashboard has defaults; explicitly null all three
		bean.setRequestType(null);
		bean.setMetric(null);
		bean.setPeriod(null);

		ChartData cd = model.getChartData();

		assertNotNull(cd);
		assertEquals("No Data", cd.getTitle());
	}

	// --- empty monitoring data path ---

	@Test
	void testGetChartDataWithAllRequestTypeAndNoMonitoringDataReturnsNoData() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentHour);

		ChartData cd = model.getChartData();

		assertNotNull(cd);
		// Monitoring has no data → "No Data Available" label + "[No Data]" in title
		assertTrue(cd.getTitle().contains("No Data"), "title should contain 'No Data'");
		assertHasNoDataLabel(cd);
	}

	@Test
	void testGetChartDataWithSpecificRequestTypeAndNoMonitoringDataReturnsNoData() {
		bean.setRequestType(RequestType.A);
		bean.setMetric(Metric.requestCPUUtilisation);
		bean.setPeriod(Period.currentDay);

		ChartData cd = model.getChartData();

		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"), "title should contain 'No Data'");
		assertHasNoDataLabel(cd);
	}

	// --- metric label coverage ---

	@Test
	void testGetChartDataElapsedRequestTimeNoData() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentMinute);

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	@Test
	void testGetChartDataRequestCPUUtilisationNoData() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.requestCPUUtilisation);
		bean.setPeriod(Period.currentMinute);

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	@Test
	void testGetChartDataSystemCPUUsageNoData() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.systemCPUUsage);
		bean.setPeriod(Period.currentWeek);

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	@Test
	void testGetChartDataSystemRAMUsageNoData() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.systemRAMUsage);
		bean.setPeriod(Period.currentYear);

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	// --- period coverage ---

	@Test
	void testGetChartDataCurrentDayPeriodNoData() {
		bean.setRequestType(RequestType.S);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentDay);

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	@Test
	void testGetChartDataCurrentWeekPeriodNoData() {
		bean.setRequestType(RequestType.E);
		bean.setMetric(Metric.systemRAMUsage);
		bean.setPeriod(Period.currentWeek);

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	@Test
	void testGetChartDataCurrentYearPeriodNoData() {
		bean.setRequestType(RequestType.C);
		bean.setMetric(Metric.systemCPUUsage);
		bean.setPeriod(Period.currentYear);

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	// --- topN default ---

	@Test
	void testGetChartDataNullTopNUsesDefault() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentHour);
		bean.setTopN(null); // should default to 10

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		// Title includes "Top 10" when topN is null
		assertTrue(cd.getTitle().contains("10"), "default topN should be 10");
	}

	@Test
	void testGetChartDataCustomTopN() {
		bean.setRequestType(RequestType.all);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentHour);
		bean.setTopN(Integer.valueOf(5));

		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("5"), "custom topN should appear in title");
	}

	// --- all request type codes ---

	@Test
	void testGetChartDataActionRequestTypeNoData() {
		bean.setRequestType(RequestType.A);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentHour);
		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	@Test
	void testGetChartDataDeleteRequestTypeNoData() {
		bean.setRequestType(RequestType.D);
		bean.setMetric(Metric.elapsedRequestTime);
		bean.setPeriod(Period.currentHour);
		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	@Test
	void testGetChartDataZoomOutRequestTypeNoData() {
		bean.setRequestType(RequestType.Z);
		bean.setMetric(Metric.systemRAMUsage);
		bean.setPeriod(Period.currentDay);
		ChartData cd = model.getChartData();
		assertNotNull(cd);
		assertTrue(cd.getTitle().contains("No Data"));
	}

	// --- RequestType enum helpers ---

	@Test
	void testRequestTypeFromCode() {
		assertEquals(RequestType.all, RequestType.fromCode("all"));
		assertEquals(RequestType.A, RequestType.fromCode("A"));
		assertEquals(RequestType.S, RequestType.fromCode("S"));
	}

	@Test
	void testRequestTypeFromCodeUnknownReturnsNull() {
		assertEquals(null, RequestType.fromCode("X"));
	}

	@Test
	void testRequestTypeToDomainValues() {
		List<DomainValue> dvs = RequestType.toDomainValues();
		assertNotNull(dvs);
		assertTrue(dvs.size() >= 11, "should have at least 11 request type domain values");
	}

	// --- Metric enum helpers ---

	@Test
	void testMetricFromCode() {
		assertEquals(Metric.elapsedRequestTime, Metric.fromCode("t"));
		assertEquals(Metric.systemRAMUsage, Metric.fromCode("r"));
	}

	@Test
	void testMetricFromCodeUnknownReturnsNull() {
		assertEquals(null, Metric.fromCode("z"));
	}

	@Test
	void testMetricToDomainValues() {
		List<DomainValue> dvs = Metric.toDomainValues();
		assertNotNull(dvs);
		assertEquals(4, dvs.size());
	}

	// --- Period enum helpers ---

	@Test
	void testPeriodFromCode() {
		assertEquals(Period.currentMinute, Period.fromCode("m"));
		assertEquals(Period.currentHour, Period.fromCode("h"));
		assertEquals(Period.currentDay, Period.fromCode("d"));
		assertEquals(Period.currentWeek, Period.fromCode("w"));
		assertEquals(Period.currentYear, Period.fromCode("y"));
	}

	@Test
	void testPeriodFromCodeUnknownReturnsNull() {
		assertEquals(null, Period.fromCode("z"));
	}

	@Test
	void testPeriodToDomainValues() {
		List<DomainValue> dvs = Period.toDomainValues();
		assertNotNull(dvs);
		assertEquals(5, dvs.size());
	}

	// --- helpers ---

	private static void assertHasNoDataLabel(ChartData cd) {
		List<String> labels = cd.getLabels();
		assertNotNull(labels, "labels should not be null");
		assertTrue(labels.contains("No Data Available"), "labels should contain 'No Data Available'");
	}
}
