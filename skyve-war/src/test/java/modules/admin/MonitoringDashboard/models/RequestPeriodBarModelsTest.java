package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for all concrete AbstractRequestPeriodBarChartModel implementations.
 */
@SuppressWarnings("static-method")
class RequestPeriodBarModelsTest {

	@Test
	void cpuPeriodBarChartLabelReturnsAvgCpu() {
		assertEquals("Average CPU Utilisation (ms)", new RequestCpuUtilisationPeriodBarModel().getChartLabel());
	}

	@Test
	void cpuPeriodBarChartTitleContainsRequestDescription() {
		String title = new RequestCpuUtilisationPeriodBarModel().getChartTitle("testRequest");
		assertTrue(title.contains("testRequest"));
	}

	@Test
	void cpuPeriodBarChartColourIsNotNull() {
		assertNotNull(new RequestCpuUtilisationPeriodBarModel().getChartColour());
	}

	@Test
	void elapsedPeriodBarChartLabelReturnsAvgElapsed() {
		assertEquals("Average Elapsed Time (ms)", new RequestElapsedTimePeriodBarModel().getChartLabel());
	}

	@Test
	void elapsedPeriodBarChartTitleContainsRequestDescription() {
		assertTrue(new RequestElapsedTimePeriodBarModel().getChartTitle("req").contains("req"));
	}

	@Test
	void heapPeriodBarChartLabelReturnsAvgHeap() {
		assertEquals("Average Heap RAM Usage (%)", new RequestHeapRamUsagePeriodBarModel().getChartLabel());
	}

	@Test
	void heapPeriodBarChartTitleContainsRequestDescription() {
		assertTrue(new RequestHeapRamUsagePeriodBarModel().getChartTitle("req").contains("req"));
	}

	@Test
	void systemCpuPeriodBarChartLabelReturnsAvgSystemCpu() {
		assertEquals("Average System CPU Usage (%)", new RequestSystemCpuUsagePeriodBarModel().getChartLabel());
	}

	@Test
	void systemCpuPeriodBarChartTitleContainsRequestDescription() {
		assertTrue(new RequestSystemCpuUsagePeriodBarModel().getChartTitle("req").contains("req"));
	}
}
