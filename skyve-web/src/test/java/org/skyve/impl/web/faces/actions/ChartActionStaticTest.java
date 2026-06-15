package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.awt.Color;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterEach;
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
import org.skyve.impl.web.faces.charts.PrimeFacesChartPostProcessor;
import org.skyve.metadata.view.model.chart.ChartData;

@SuppressWarnings("static-method")
class ChartActionStaticTest {

	private static final AtomicReference<ChartModel> LAST_PROCESSED_MODEL = new AtomicReference<>();

	public static class TestPrimeFacesChartPostProcessor implements PrimeFacesChartPostProcessor<ChartModel> {
		@Override
		public void process(ChartModel model) {
			LAST_PROCESSED_MODEL.set(model);
			model.setExtender("TEST.PROCESSED");
		}
	}

	@AfterEach
	void afterEach() {
		LAST_PROCESSED_MODEL.set(null);
	}

	private static ChartData minimalChartData() {
		ChartData data = new ChartData();
		data.setLabel("Test Label");
		data.setValues(Arrays.asList(Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)));
		data.setLabels(Arrays.asList("A", "B", "C"));
		data.setBackground(Color.BLUE);
		data.setBorder(Color.RED);
		data.setPrimeFacesChartPostProcessorClassName(TestPrimeFacesChartPostProcessor.class.getName());
		return data;
	}

	private static ChartData multiColorChartData() {
		ChartData data = new ChartData();
		data.setValues(Arrays.asList(Integer.valueOf(10), Integer.valueOf(20), Integer.valueOf(30)));
		data.setLabels(Arrays.asList("X", "Y", "Z"));
		data.setBackgrounds(Arrays.asList(Color.RED, Color.GREEN, Color.BLUE));
		data.setBorders(Arrays.asList(Color.DARK_GRAY, Color.DARK_GRAY, Color.DARK_GRAY));
		data.setPrimeFacesChartPostProcessorClassName(TestPrimeFacesChartPostProcessor.class.getName());
		return data;
	}

	@Test
	void pfChartModelWithBarTypeReturnsBarChartModelAndProcessesIt() {
		ChartData data = minimalChartData();

		ChartModel result = ChartAction.pfChartModel(ChartType.bar, data);

		assertNotNull(result);
		assertInstanceOf(BarChartModel.class, result);
		assertEquals("TEST.PROCESSED", result.getExtender());
		assertEquals(result, LAST_PROCESSED_MODEL.get());
	}

	@Test
	void pfChartModelWithTransparentBarColoursStillBuilds() {
		ChartData data = minimalChartData();
		data.setBackground(new Color(10, 20, 30, 0));
		data.setBorder(new Color(40, 50, 60, 0));

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
	void pfChartModelWithPieTypeReturnsPieChartModel() {
		ChartData data = multiColorChartData();

		ChartModel result = ChartAction.pfChartModel(ChartType.pie, data);

		assertNotNull(result);
		assertInstanceOf(PieChartModel.class, result);
	}

	@Test
	void pfChartModelWithPieTypeAndNullColoursStillBuilds() {
		ChartData data = new ChartData();
		data.setValues(Arrays.asList(Integer.valueOf(10), Integer.valueOf(20), Integer.valueOf(30)));
		data.setLabels(Arrays.asList("X", "Y", "Z"));
		data.setPrimeFacesChartPostProcessorClassName(TestPrimeFacesChartPostProcessor.class.getName());

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
	void pfChartModelWithRadarTypeReturnsRadarChartModel() {
		ChartData data = minimalChartData();

		ChartModel result = ChartAction.pfChartModel(ChartType.radar, data);

		assertNotNull(result);
		assertInstanceOf(RadarChartModel.class, result);
	}

	@Test
	void pfChartModelWithTitleSetsOptionsAndExtender() {
		ChartData data = minimalChartData();
		data.setTitle("Bar Chart Title");

		ChartModel result = ChartAction.pfChartModel(ChartType.bar, data);

		assertNotNull(result);
		assertInstanceOf(BarChartModel.class, result);
		assertEquals("TEST.PROCESSED", result.getExtender());
	}

	@Test
	void pfChartModelWithUnsupportedTypeThrows() {
		ChartData data = minimalChartData();

		assertThrows(IllegalArgumentException.class, () -> ChartAction.pfChartModel(null, data));
	}

	@Test
	void postProcessorClassMissingFailsCleanly() {
		ChartData data = minimalChartData();
		data.setPrimeFacesChartPostProcessorClassName("no.such.TestProcessor");

		assertThrows(IllegalStateException.class, () -> ChartAction.pfChartModel(ChartType.bar, data));
	}

	@Test
	void chartModelBuilderDoesNotThrowForAllSupportedTypes() {
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.bar, minimalChartData()));
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.horizontalBar, minimalChartData()));
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.doughnut, multiColorChartData()));
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.line, minimalChartData()));
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.lineArea, minimalChartData()));
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.pie, multiColorChartData()));
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.polarArea, multiColorChartData()));
		assertDoesNotThrow(() -> ChartAction.pfChartModel(ChartType.radar, minimalChartData()));
	}
}