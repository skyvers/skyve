package org.skyve.impl.generate.charts;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.List;

import org.jfree.chart.JFreeChart;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.metadata.view.model.chart.ChartData;

@SuppressWarnings("static-method")
class JFreeChartGeneratorTest {

	/**
	 * No-op post processor used in tests to bypass {@code CORE.getCustomer()}.
	 * Must be {@code public static} so the production-code class loader can
	 * instantiate it by name.
	 */
	public static class NoOpPostProcessor implements JFreeChartPostProcessor {
		@Override
		public void process(JFreeChart chart) {
			// intentionally empty
		}
	}

	/** Chart data suitable for category-based chart types (bar, line, etc.). */
	private static ChartData categoryData() {
		ChartData data = new ChartData();
		data.setTitle("Test Chart");
		data.setLabel("Series 1");
		data.setLabels(List.of("Alpha", "Beta", "Gamma"));
		data.setValues(List.of(10, 25, 15));
		data.setBackground(Color.BLUE);
		data.setBorder(Color.BLACK);
		data.setJFreeChartPostProcessorClassName(NoOpPostProcessor.class.getName());
		return data;
	}

	/** Chart data suitable for pie / doughnut chart types. */
	private static ChartData pieData() {
		ChartData data = new ChartData();
		data.setTitle("Pie Chart");
		data.setLabel("Series");
		data.setLabels(List.of("X", "Y"));
		data.setValues(List.of(40, 60));
		data.setBackground(Color.RED);
		data.setBorder(Color.BLACK);
		data.setBackgrounds(List.of(Color.RED, Color.BLUE));
		data.setBorders(List.of(Color.BLACK, Color.GRAY));
		data.setJFreeChartPostProcessorClassName(NoOpPostProcessor.class.getName());
		return data;
	}

	/** Chart data suitable for polarArea (requires parallel backgrounds/borders/labels/values lists). */
	private static ChartData polarData() {
		ChartData data = new ChartData();
		data.setTitle("Polar Chart");
		data.setLabel("Series");
		data.setLabels(List.of("P", "Q"));
		data.setValues(List.of(30, 70));
		data.setBackground(Color.GREEN);
		data.setBorder(Color.DARK_GRAY);
		data.setBackgrounds(List.of(Color.GREEN, Color.ORANGE));
		data.setBorders(List.of(Color.DARK_GRAY, Color.MAGENTA));
		data.setJFreeChartPostProcessorClassName(NoOpPostProcessor.class.getName());
		return data;
	}

	// ---- chart(ChartType) dispatch ----

	@Test
	void chartReturnsNonNullForBarType() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.bar);
		assertNotNull(chart);
	}

	@Test
	void chartBarHasExpectedTitle() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.bar);
		assertEquals("Test Chart", chart.getTitle().getText());
	}

	@Test
	void chartReturnsNonNullForHorizontalBar() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.horizontalBar);
		assertNotNull(chart);
	}

	@Test
	void chartReturnsNonNullForLine() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.line);
		assertNotNull(chart);
	}

	@Test
	void chartReturnsNonNullForLineArea() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.lineArea);
		assertNotNull(chart);
	}

	@Test
	void chartReturnsNonNullForPie() {
		JFreeChartGenerator gen = new JFreeChartGenerator(pieData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.pie);
		assertNotNull(chart);
	}

	@Test
	void chartReturnsNonNullForDoughnut() {
		JFreeChartGenerator gen = new JFreeChartGenerator(pieData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.doughnut);
		assertNotNull(chart);
	}

	@Test
	void chartReturnsNonNullForPolarArea() {
		JFreeChartGenerator gen = new JFreeChartGenerator(polarData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.polarArea);
		assertNotNull(chart);
	}

	@Test
	void chartReturnsNonNullForRadar() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		JFreeChart chart = gen.chart(ChartType.radar);
		assertNotNull(chart);
	}

	// ---- direct chart-type methods ----

	@Test
	void barReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		assertNotNull(gen.bar());
	}

	@Test
	void horizontalBarReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		assertNotNull(gen.horizontalBar());
	}

	@Test
	void lineReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		assertNotNull(gen.line());
	}

	@Test
	void lineAreaReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		assertNotNull(gen.lineArea());
	}

	@Test
	void pieReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(pieData(), 800, 600);
		assertNotNull(gen.pie());
	}

	@Test
	void doughnutReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(pieData(), 800, 600);
		assertNotNull(gen.doughnut());
	}

	@Test
	void polarAreaReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(polarData(), 800, 600);
		assertNotNull(gen.polarArea());
	}

	@Test
	void radarReturnsNonNullChart() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 800, 600);
		assertNotNull(gen.radar());
	}

	// ---- image methods ----

	@Test
	void imageFromChartReturnsBufferedImageWithCorrectDimensions() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 120, 80);
		JFreeChart chart = gen.bar();
		BufferedImage img = gen.image(chart);
		assertNotNull(img);
		assertEquals(120, img.getWidth());
		assertEquals(80, img.getHeight());
	}

	@Test
	void imageFromTypeReturnsBufferedImageForBar() {
		JFreeChartGenerator gen = new JFreeChartGenerator(categoryData(), 100, 100);
		BufferedImage img = gen.image(ChartType.bar);
		assertNotNull(img);
	}

	// ---- postProcess: non-null class name branch ----

	@Test
	void postProcessWithValidClassNameInvokesProcessor() {
		// Data with explicit post processor class name → postProcess uses it without CORE
		ChartData data = categoryData(); // already has NoOpPostProcessor set
		JFreeChartGenerator gen = new JFreeChartGenerator(data, 800, 600);
		// Should not throw
		JFreeChart chart = gen.chart(ChartType.bar);
		assertNotNull(chart);
	}

	@Test
	void postProcessWithInvalidClassNameThrowsIllegalStateException() {
		ChartData data = categoryData();
		data.setJFreeChartPostProcessorClassName("com.example.NonExistentClass");
		JFreeChartGenerator gen = new JFreeChartGenerator(data, 800, 600);
		assertThrows(IllegalStateException.class, () -> gen.chart(ChartType.bar));
	}
}
