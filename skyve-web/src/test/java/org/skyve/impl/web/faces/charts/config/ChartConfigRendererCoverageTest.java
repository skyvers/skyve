package org.skyve.impl.web.faces.charts.config;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.primefaces.model.charts.ChartModel;
import org.primefaces.model.charts.bar.BarChartModel;
import org.primefaces.model.charts.donut.DonutChartModel;
import org.primefaces.model.charts.hbar.HorizontalBarChartModel;
import org.primefaces.model.charts.line.LineChartModel;
import org.primefaces.model.charts.pie.PieChartModel;
import org.primefaces.model.charts.polar.PolarAreaChartModel;
import org.primefaces.model.charts.radar.RadarChartModel;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;

@SuppressWarnings("static-method")
class ChartConfigRendererCoverageTest {
	@Test
	void configRendersJsonForAllSupportedChartTypes() throws Exception {
		assertJson(ChartType.bar, new BarChartModel());
		assertJson(ChartType.horizontalBar, new HorizontalBarChartModel());
		assertJson(ChartType.doughnut, new DonutChartModel());
		assertJson(ChartType.line, new LineChartModel());
		assertJson(ChartType.lineArea, new LineChartModel());
		assertJson(ChartType.pie, new PieChartModel());
		assertJson(ChartType.polarArea, new PolarAreaChartModel());
		assertJson(ChartType.radar, new RadarChartModel());
	}

	private static void assertJson(ChartType type, ChartModel model) throws Exception {
		String config = ChartConfigRenderer.config(type, model);
		assertTrue(config.startsWith("{"));
		assertTrue(config.endsWith("}"));
	}
}
