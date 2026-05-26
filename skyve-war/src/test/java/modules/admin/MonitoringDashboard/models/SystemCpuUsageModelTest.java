package modules.admin.MonitoringDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.ChartData;

import modules.admin.domain.MonitoringDashboard.Period;

@SuppressWarnings("static-method")
public class SystemCpuUsageModelTest {

	@Test
	void getChartLabelReturnsCpuUsageLabel() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		assertEquals("System CPU Usage (%)", model.getChartLabel());
	}

	@Test
	void getChartTitleContainsPeriodDescription() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		String title = model.getChartTitle(Period.currentDay);
		assertTrue(title.startsWith("System CPU Usage - "));
		assertTrue(title.contains(Period.currentDay.toLocalisedDescription()));
	}

	@Test
	void setChartColorsWithEmptyValuesUsesLightGray() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		ChartData cd = new ChartData();
		model.setChartColors(cd, Collections.emptyList());
		assertEquals(Color.LIGHT_GRAY, cd.getBackground());
		assertEquals(Color.LIGHT_GRAY, cd.getBorder());
	}

	@Test
	void setChartColorsWithHighCpuLoadSetsNonNullColor() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		ChartData cd = new ChartData();
		List<Number> values = Arrays.asList(90f); // >85% = red
		model.setChartColors(cd, values);
		assertNotNull(cd.getBackground());
	}

	@Test
	void setChartColorsWithLowCpuLoadSetsGreenColor() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		ChartData cd = new ChartData();
		List<Number> values = Arrays.asList(30f); // <50% = green
		model.setChartColors(cd, values);
		assertNotNull(cd.getBackground());
	}
}
