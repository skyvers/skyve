package org.skyve.impl.generate.charts;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.util.List;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PolarPlot;
import org.jfree.chart.plot.SpiderWebPlot;
import org.jfree.chart.renderer.DefaultPolarItemRenderer;
import org.jfree.chart.renderer.category.AbstractCategoryItemRenderer;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.chart.renderer.category.LineAndShapeRenderer;
import org.jfree.chart.renderer.category.StandardBarPainter;
import org.jfree.chart.title.LegendTitle;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.CategoryToPieDataset;
import org.jfree.data.category.DefaultCategoryDataset;
import org.jfree.data.general.PieDataset;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.util.TableOrder;
import org.skyve.CORE;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.chart.ChartData;

public class JFreeChartGenerator {
	private static final String NO_DATA_AVAILABLE = "No data available";

	private static final String THEME_FONT_NAME = "Arial";
	private static final int THEME_NORMAL_FONT_SIZE = 12;
	private static final int THEME_HEADER_FONT_SIZE = 14; 
	private static final Font THEME_LEGEND_FONT = new Font(THEME_FONT_NAME, Font.PLAIN, THEME_NORMAL_FONT_SIZE);
	private static final Font THEME_TITLE_FONT = new Font(THEME_FONT_NAME, Font.BOLD, THEME_NORMAL_FONT_SIZE);
	private static final Font THEME_RANGE_FONT = new Font(THEME_FONT_NAME, Font.PLAIN, THEME_HEADER_FONT_SIZE);
	private static final Font THEME_DOMAIN_FONT = new Font(THEME_FONT_NAME, Font.PLAIN, THEME_NORMAL_FONT_SIZE);

	private ChartData data = null;
	private int pixelWidth = 0;
	private int pixelHeight = 0;

	public JFreeChartGenerator(ChartData data, int pixelWidth, int pixelHeight) {
		this.data = data;
		this.pixelWidth = pixelWidth;
		this.pixelHeight = pixelHeight;
	}

	public JFreeChart chart(ChartType type) {
		JFreeChart result = null;
		
		if (ChartType.bar.equals(type)) {
			result = bar();
		}
		else if (ChartType.doughnut.equals(type)) {
			result = doughnut();
		}
		else if (ChartType.horizontalBar.equals(type)) {
			result = horizontalBar();
		}
		else if (ChartType.line.equals(type)) {
			result = line();
		}
		else if (ChartType.lineArea.equals(type)) {
			result = lineArea();
		}
		else if (ChartType.pie.equals(type)) {
			result = pie();
		}
		else if (ChartType.polarArea.equals(type)) {
			result = polarArea();
		}
		else if (ChartType.radar.equals(type)) {
			result = radar();
		}
		else {
			throw new IllegalArgumentException(type + " is not supported");
		}
		
		return result;
	}
	
	public BufferedImage image(JFreeChart chart) {
		return chart.createBufferedImage(pixelWidth, pixelHeight);
	}

	public BufferedImage image(ChartType type) {
		return image(chart(type));
	}

	public JFreeChart bar() {
		CategoryDataset dataSet = dataSet();
		JFreeChart result = ChartFactory.createBarChart(data.getTitle(),
															"",
															"",
															dataSet,
															PlotOrientation.VERTICAL,
															true,
															false,
															false);
		configureCategoryChart(result);
		// Set the bar painting to a flat fill 
		((BarRenderer) ((CategoryPlot) result.getPlot()).getRenderer()).setBarPainter(new StandardBarPainter());

		postProcess(result);
		return result;
	}
	
	public JFreeChart horizontalBar() {
		CategoryDataset dataSet = dataSet();
		JFreeChart result = ChartFactory.createBarChart(data.getTitle(),
															"",
															"",
															dataSet,
															PlotOrientation.HORIZONTAL,
															true,
															false,
															false);
		configureCategoryChart(result);
		// Set the bar painting to a flat fill 
		((BarRenderer) ((CategoryPlot) result.getPlot()).getRenderer()).setBarPainter(new StandardBarPainter());

		postProcess(result);
		return result;
	}
	
	public JFreeChart lineArea() {
		CategoryDataset dataSet = dataSet();
		JFreeChart result = ChartFactory.createAreaChart(data.getTitle(),
															"",
															"",
															dataSet,
															PlotOrientation.VERTICAL,
															true,
															false,
															false);
		configureCategoryChart(result);

		postProcess(result);
		return result;
	}
	
	public JFreeChart line() {
		CategoryDataset dataSet = dataSet();
		JFreeChart result = ChartFactory.createLineChart(data.getTitle(),
															"",
															"",
															dataSet,
															PlotOrientation.VERTICAL,
															true,
															false,
															false);
		configureCategoryChart(result);
		LineAndShapeRenderer renderer = (LineAndShapeRenderer) result.getCategoryPlot().getRenderer();
		renderer.setBaseShapesVisible(true);
		renderer.setBaseShapesFilled(true);

		postProcess(result);
		return result;
	}
	
	public JFreeChart pie() {
		PieDataset dataSet = pieDataSet();
		JFreeChart result = ChartFactory.createPieChart(data.getTitle(), dataSet, true, false, false);
		configurePieChart(result);

		postProcess(result);
		return result;
	}
	
	public JFreeChart doughnut() {
		PieDataset dataSet = pieDataSet();
		JFreeChart result = ChartFactory.createRingChart(data.getTitle(), dataSet, true, false, false);
		configurePieChart(result);

		postProcess(result);
		return result;
	}

	public JFreeChart polarArea() {
		XYSeriesCollection dataSet = new XYSeriesCollection();
		dataSet.setNotify(false);
		
		List<String> labels = data.getLabels();
		List<Number> values = data.getValues();
		
		for (int i = 0, l = values.size(); i < l; i++) {
			XYSeries series = new XYSeries(labels.get(i));
			series.setNotify(false);
			series.add(0, 0);
			series.add(i * 360 / l, values.get(i));
			series.add((i + 1) * 360 / l, values.get(i));
			dataSet.addSeries(series);
		}

		JFreeChart result = ChartFactory.createPolarChart(data.getTitle(), dataSet, true, false, false);
		result.setBackgroundPaint(null);
		result.setBorderVisible(false);
		
	    PolarPlot plot = (PolarPlot) result.getPlot();
		plot.setBackgroundPaint(null);
		plot.setAngleGridlinesVisible(true);
		plot.setAngleGridlinePaint(Color.LIGHT_GRAY);
		plot.setRadiusGridlinesVisible(true);
		plot.setRadiusGridlinePaint(Color.LIGHT_GRAY);
		plot.setNoDataMessage(NO_DATA_AVAILABLE);
		plot.setOutlineVisible(false);
		plot.setAngleLabelsVisible(false);

		List<Color> backgrounds = data.getBackgrounds();
		List<Color> borders = data.getBorders();
		
		DefaultPolarItemRenderer renderer = (DefaultPolarItemRenderer) plot.getRenderer();
		for (int i = 0, l = values.size(); i < l; i++) {
			renderer.setSeriesFilled(i, true);
			renderer.setSeriesShape(i, null);
			renderer.setSeriesFillPaint(i, backgrounds.get(i));
			renderer.setSeriesOutlinePaint(i, borders.get(i));
		}

		renderer.setItemLabelAnchorOffset(0);
		
		LegendTitle legend = result.getLegend();
		legend.setItemFont(THEME_LEGEND_FONT);

		TextTitle title = result.getTitle();
		title.setFont(THEME_TITLE_FONT);

		postProcess(result);
		return result;
	}
	
	public JFreeChart radar() {
		CategoryDataset dataSet = dataSet();
	    SpiderWebPlot plot = new SpiderWebPlot(dataSet);
		plot.setBackgroundPaint(null);
		plot.setNoDataMessage(NO_DATA_AVAILABLE);
		plot.setOutlineVisible(false);
		plot.setAxisLinePaint(Color.LIGHT_GRAY);
	    
	    JFreeChart result = new JFreeChart(data.getTitle(), THEME_TITLE_FONT, plot, true);
	    result.setBackgroundPaint(null);
	    result.setBorderVisible(false);
	    
		LegendTitle legend = result.getLegend();
		legend.setItemFont(THEME_LEGEND_FONT);

		postProcess(result);
		return result;
	}

	private CategoryDataset dataSet() {
		DefaultCategoryDataset result = new DefaultCategoryDataset();
		
		result.setNotify(false);
		String rowKey = data.getLabel();
		List<String> labels = data.getLabels();
		List<Number> values = data.getValues();
		for (int i = 0, l = values.size(); i < l; i++) {
			String columnKey = labels.get(i);
			result.addValue(values.get(i), rowKey, columnKey);
		}

		return result;
	}
	
	private PieDataset pieDataSet() {
		return new CategoryToPieDataset(dataSet(), TableOrder.BY_ROW, 0);
	}
	
	private void configureCategoryChart(JFreeChart chart) {
		chart.setBackgroundPaint(null);
	    chart.setBorderVisible(false);
		
		CategoryPlot plot = (CategoryPlot) chart.getPlot();
		plot.setRangeGridlinesVisible(true);
		plot.setDomainGridlinesVisible(true);
		plot.setRangeGridlinePaint(Color.LIGHT_GRAY);
		plot.setDomainGridlinePaint(Color.LIGHT_GRAY);
		plot.setBackgroundPaint(null);
		plot.setNoDataMessage(NO_DATA_AVAILABLE);
		plot.setOutlineVisible(false);

		AbstractCategoryItemRenderer renderer = (AbstractCategoryItemRenderer) plot.getRenderer();
		renderer.setSeriesPaint(0, data.getBackground());
		renderer.setSeriesOutlinePaint(0, data.getBorder());
		renderer.setSeriesOutlineStroke(0, new BasicStroke(1));
		renderer.setSeriesFillPaint(0, data.getBackground());

		renderer.setItemLabelAnchorOffset(0);
		
		LegendTitle legend = chart.getLegend();
		if (legend != null) {
			legend.setItemFont(THEME_LEGEND_FONT);
		}

		TextTitle title = chart.getTitle();
		if (title != null) {
			title.setFont(THEME_TITLE_FONT);
		}

		plot.getDomainAxis().setLabelFont(THEME_DOMAIN_FONT);
		plot.getRangeAxis().setLabelFont(THEME_RANGE_FONT);
	}
	
	private void configurePieChart(JFreeChart chart) {
		chart.setBackgroundPaint(null);
	    chart.setBorderVisible(false);

		PiePlot plot = (PiePlot) chart.getPlot();
		plot.setBackgroundPaint(null);
		plot.setNoDataMessage(NO_DATA_AVAILABLE);
		plot.setOutlineVisible(false);

		List<String> labels = data.getLabels();
		List<Color> backgrounds = data.getBackgrounds();
		List<Color> borders = data.getBorders();
		
		for (int i = 0, l = backgrounds.size(); i < l; i++) {
			String label = labels.get(i);
			plot.setSectionPaint(label, backgrounds.get(i));
			plot.setSectionOutlinePaint(label, borders.get(i));
		}
		
		LegendTitle legend = chart.getLegend();
		if (legend != null) {
			legend.setItemFont(THEME_LEGEND_FONT);
		}

		TextTitle title = chart.getTitle();
		if (title != null) {
			title.setFont(THEME_TITLE_FONT);
		}
	}
	
	private void postProcess(JFreeChart chart) {
		String postProcessor = data.getJFreeChartPostProcessorClassName();
		if (postProcessor == null) {
			Customer c = CORE.getCustomer();
			postProcessor = c.getJFreeChartPostProcessorClassName();
		}
		if (postProcessor != null) {
			try {
				JFreeChartPostProcessor instance = (JFreeChartPostProcessor) Thread.currentThread().getContextClassLoader().loadClass(postProcessor).newInstance();
				instance.process(chart);
			}
			catch (Exception e) {
				throw new IllegalStateException("Could not create chart post processor", e);
			}
		}
	}
}
