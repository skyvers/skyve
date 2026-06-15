package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests covering all concrete AbstractDocumentChartModel implementations.
 */
@SuppressWarnings("static-method")
class DocumentModelsTest {

	// DocumentCreateCpuUtilisationModel

	@Test
	void createCpuChartLabelReturnsCpuUtilisation() {
		assertEquals("CPU Utilisation (%)", new DocumentCreateCpuUtilisationModel().getChartLabel());
	}

	@Test
	void createCpuChartTitleContainsSelectedDocument() {
		String title = new DocumentCreateCpuUtilisationModel().getChartTitle("MyDoc");
		assertTrue(title.contains("MyDoc"));
		assertTrue(title.contains("Create"));
	}

	@Test
	void createCpuChartColorIsNotNull() {
		assertNotNull(new DocumentCreateCpuUtilisationModel().getChartColor());
	}

	// DocumentCreateElapsedTimeModel

	@Test
	void createElapsedChartLabelReturnsElapsedTime() {
		assertEquals("Elapsed Time (ms)", new DocumentCreateElapsedTimeModel().getChartLabel());
	}

	@Test
	void createElapsedChartTitleContainsSelectedDocument() {
		assertTrue(new DocumentCreateElapsedTimeModel().getChartTitle("ADoc").contains("ADoc"));
	}

	@Test
	void createElapsedChartColorIsNotNull() {
		assertNotNull(new DocumentCreateElapsedTimeModel().getChartColor());
	}

	// DocumentCreateHeapRamUsageModel

	@Test
	void createHeapChartLabelReturnsHeapRam() {
		assertEquals("Heap RAM Usage (%)", new DocumentCreateHeapRamUsageModel().getChartLabel());
	}

	@Test
	void createHeapChartTitleContainsSelectedDocument() {
		assertTrue(new DocumentCreateHeapRamUsageModel().getChartTitle("ADoc").contains("ADoc"));
	}

	// DocumentCreateSystemCpuUsageModel

	@Test
	void createSystemCpuChartLabelReturnsSystemCpu() {
		assertEquals("System CPU Usage (%)", new DocumentCreateSystemCpuUsageModel().getChartLabel());
	}

	@Test
	void createSystemCpuChartTitleContainsSelectedDocument() {
		assertTrue(new DocumentCreateSystemCpuUsageModel().getChartTitle("ADoc").contains("ADoc"));
	}

	// DocumentEditCpuUtilisationModel

	@Test
	void editCpuChartLabelReturnsCpuUtilisation() {
		assertEquals("CPU Utilisation (%)", new DocumentEditCpuUtilisationModel().getChartLabel());
	}

	@Test
	void editCpuChartTitleContainsEditAndDocument() {
		String title = new DocumentEditCpuUtilisationModel().getChartTitle("EditDoc");
		assertTrue(title.contains("Edit") && title.contains("EditDoc"));
	}

	// DocumentEditElapsedTimeModel

	@Test
	void editElapsedChartLabelReturnsElapsedTime() {
		assertEquals("Elapsed Time (ms)", new DocumentEditElapsedTimeModel().getChartLabel());
	}

	@Test
	void editElapsedChartTitleContainsSelectedDocument() {
		assertTrue(new DocumentEditElapsedTimeModel().getChartTitle("D").contains("D"));
	}

	// DocumentEditHeapRamUsageModel

	@Test
	void editHeapChartLabelReturnsHeapRam() {
		assertNotNull(new DocumentEditHeapRamUsageModel().getChartLabel());
	}

	@Test
	void editHeapChartTitleContainsSelectedDocument() {
		assertTrue(new DocumentEditHeapRamUsageModel().getChartTitle("D").contains("D"));
	}

	// DocumentEditSystemCpuUsageModel

	@Test
	void editSystemCpuChartLabelReturnsSystemCpu() {
		assertNotNull(new DocumentEditSystemCpuUsageModel().getChartLabel());
	}

	@Test
	void editSystemCpuChartTitleContainsSelectedDocument() {
		assertTrue(new DocumentEditSystemCpuUsageModel().getChartTitle("D").contains("D"));
	}
}
