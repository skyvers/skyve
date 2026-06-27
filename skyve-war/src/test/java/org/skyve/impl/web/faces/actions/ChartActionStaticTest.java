package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.awt.Color;
import java.util.Arrays;

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
import org.skyve.metadata.view.model.chart.ChartData;

import modules.test.AbstractSkyveTest;

/**
 * Tests for ChartAction.pfChartModel() — a public static method
 * that converts ChartData + ChartType to a PrimeFaces ChartModel.
 * Needs AbstractSkyveTest because postProcess() calls CORE.getCustomer().
 */
@SuppressWarnings("static-method")
class ChartActionStaticTest extends AbstractSkyveTest {

	private static ChartData minimalChartData() {
		ChartData data = new ChartData();
		data.setLabel("Test Label");
		data.setValues(Arrays.asList(Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)));
		data.setLabels(Arrays.asList("A", "B", "C"));
		data.setBackground(Color.BLUE);
		data.setBorder(Color.RED);
		return data;
	}

	private static ChartData multiColorChartData() {
		ChartData data = new ChartData();
		data.setValues(Arrays.asList(Integer.valueOf(10), Integer.valueOf(20), Integer.valueOf(30)));
		data.setLabels(Arrays.asList("X", "Y", "Z"));
		data.setBackgrounds(Arrays.asList(Color.RED, Color.GREEN, Color.BLUE));
		data.setBorders(Arrays.asList(Color.DARK_GRAY, Color.DARK_GRAY, Color.DARK_GRAY));
		return data;
	}

	@Test
	void pfChartModelWithBarTypeReturnsBarChartModel() {
		ChartData data = minimalChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.bar, data);
		assertNotNull(result);
		assertInstanceOf(BarChartModel.class, result);
	}

	@Test
	void pfChartModelWithBarTypeAndTitleSetsOptions() {
		ChartData data = minimalChartData();
		data.setTitle("Bar Chart Title");
		ChartModel result = ChartAction.pfChartModel(ChartType.bar, data);
		assertNotNull(result);
		assertInstanceOf(BarChartModel.class, result);
	}

	@Test
	void pfChartModelWithHorizontalBarTypeReturnsHorizontalBarChartModel() {
		ChartData data = minimalChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.horizontalBar, data);
		assertNotNull(result);
		assertInstanceOf(HorizontalBarChartModel.class, result);
	}

	@Test
	void pfChartModelWithDoughnutTypeReturnsDoughnutChartModel() {
		ChartData data = multiColorChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.doughnut, data);
		assertNotNull(result);
		assertInstanceOf(DonutChartModel.class, result);
	}

	@Test
	void pfChartModelWithDoughnutTypeAndTitleSetsOptions() {
		ChartData data = multiColorChartData();
		data.setTitle("Doughnut Title");
		ChartModel result = ChartAction.pfChartModel(ChartType.doughnut, data);
		assertNotNull(result);
		assertInstanceOf(DonutChartModel.class, result);
	}

	@Test
	void pfChartModelWithLineTypeReturnsLineChartModel() {
		ChartData data = minimalChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.line, data);
		assertNotNull(result);
		assertInstanceOf(LineChartModel.class, result);
	}

	@Test
	void pfChartModelWithLineAreaTypeReturnsLineChartModel() {
		ChartData data = minimalChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.lineArea, data);
		assertNotNull(result);
		assertInstanceOf(LineChartModel.class, result);
	}

	@Test
	void pfChartModelWithLineTypeAndTitleSetsOptions() {
		ChartData data = minimalChartData();
		data.setTitle("Line Title");
		ChartModel result = ChartAction.pfChartModel(ChartType.line, data);
		assertNotNull(result);
		assertInstanceOf(LineChartModel.class, result);
	}

	@Test
	void pfChartModelWithPieTypeReturnsPieChartModel() {
		ChartData data = multiColorChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.pie, data);
		assertNotNull(result);
		assertInstanceOf(PieChartModel.class, result);
	}

	@Test
	void pfChartModelWithPieTypeAndTitleSetsOptions() {
		ChartData data = multiColorChartData();
		data.setTitle("Pie Title");
		ChartModel result = ChartAction.pfChartModel(ChartType.pie, data);
		assertNotNull(result);
		assertInstanceOf(PieChartModel.class, result);
	}

	@Test
	void pfChartModelWithPolarAreaTypeReturnsPolarAreaChartModel() {
		ChartData data = multiColorChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.polarArea, data);
		assertNotNull(result);
		assertInstanceOf(PolarAreaChartModel.class, result);
	}

	@Test
	void pfChartModelWithPolarAreaTypeAndTitleSetsOptions() {
		ChartData data = multiColorChartData();
		data.setTitle("PolarArea Title");
		ChartModel result = ChartAction.pfChartModel(ChartType.polarArea, data);
		assertNotNull(result);
		assertInstanceOf(PolarAreaChartModel.class, result);
	}

	@Test
	void pfChartModelWithRadarTypeReturnsRadarChartModel() {
		ChartData data = minimalChartData();
		ChartModel result = ChartAction.pfChartModel(ChartType.radar, data);
		assertNotNull(result);
		assertInstanceOf(RadarChartModel.class, result);
	}

	@Test
	void pfChartModelWithRadarTypeAndTitleSetsOptions() {
		ChartData data = minimalChartData();
		data.setTitle("Radar Title");
		ChartModel result = ChartAction.pfChartModel(ChartType.radar, data);
		assertNotNull(result);
		assertInstanceOf(RadarChartModel.class, result);
	}
}
