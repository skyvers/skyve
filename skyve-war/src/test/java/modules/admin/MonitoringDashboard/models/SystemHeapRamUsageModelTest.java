package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.ChartData;

import modules.admin.domain.MonitoringDashboard.Period;

@SuppressWarnings("static-method")
public class SystemHeapRamUsageModelTest {

	@Test
	void getChartLabelReturnsHeapRamLabel() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		assertEquals("Heap RAM Usage (%)", model.getChartLabel());
	}

	@Test
	void getChartTitleContainsPeriodDescription() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		String title = model.getChartTitle(Period.currentHour);
		assertTrue(title.startsWith("Heap RAM Usage - "));
		assertTrue(title.contains(Period.currentHour.toLocalisedDescription()));
	}

	@Test
	void setChartColorsWithEmptyValuesUsesLightGray() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		ChartData cd = new ChartData();
		model.setChartColors(cd, Collections.emptyList());
		assertEquals(Color.LIGHT_GRAY, cd.getBackground());
	}

	@Test
	void setChartColorsWithHighRamUsageSetsNonNullColor() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		ChartData cd = new ChartData();
		model.setChartColors(cd, Arrays.asList(90f)); // >85% = red
		assertNotNull(cd.getBackground());
	}
}
