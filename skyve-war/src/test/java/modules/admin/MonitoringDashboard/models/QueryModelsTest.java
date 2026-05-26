package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for all concrete AbstractQueryChartModel implementations.
 */
@SuppressWarnings("static-method")
public class QueryModelsTest {

	@Test
	void queryCpuChartLabelReturnsCpuUtilisation() {
		assertEquals("CPU Utilisation (%)", new QueryCpuUtilisationModel().getChartLabel());
	}

	@Test
	void queryCpuChartTitleContainsSelectedQuery() {
		String title = new QueryCpuUtilisationModel().getChartTitle("MyQuery");
		assertTrue(title.contains("MyQuery") && title.contains("CPU"));
	}

	@Test
	void queryCpuChartColorIsNotNull() {
		assertNotNull(new QueryCpuUtilisationModel().getChartColor());
	}

	@Test
	void queryElapsedChartLabelReturnsElapsedTime() {
		assertEquals("Elapsed Time (ms)", new QueryElapsedTimeModel().getChartLabel());
	}

	@Test
	void queryElapsedChartTitleContainsSelectedQuery() {
		assertTrue(new QueryElapsedTimeModel().getChartTitle("Q").contains("Q"));
	}

	@Test
	void queryHeapChartLabelReturnsHeapRam() {
		assertEquals("Heap RAM Usage (%)", new QueryHeapRamUsageModel().getChartLabel());
	}

	@Test
	void queryHeapChartTitleContainsSelectedQuery() {
		assertTrue(new QueryHeapRamUsageModel().getChartTitle("Q").contains("Q"));
	}

	@Test
	void querySystemCpuChartLabelIsNotNull() {
		assertNotNull(new QuerySystemCpuUsageModel().getChartLabel());
	}

	@Test
	void querySystemCpuChartTitleContainsSelectedQuery() {
		assertTrue(new QuerySystemCpuUsageModel().getChartTitle("Q").contains("Q"));
	}
}
