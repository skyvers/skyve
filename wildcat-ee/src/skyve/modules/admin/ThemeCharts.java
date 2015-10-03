package modules.admin;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.DecimalFormat;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.labels.CategoryItemLabelGenerator;
import org.jfree.chart.labels.PieSectionLabelGenerator;
import org.jfree.chart.labels.StandardCategoryItemLabelGenerator;
import org.jfree.chart.labels.StandardPieSectionLabelGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.AreaRenderer;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.chart.renderer.category.LineAndShapeRenderer;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.jdbc.JDBCCategoryDataset;
import org.jfree.data.jdbc.JDBCPieDataset;
import org.skyve.EXT;

public class ThemeCharts {

	private static final DecimalFormat themeNumericFormat = new DecimalFormat("#,##0");
	private static final String EMPTY_STRING = "";
	private static final Color themeColour = new Color(70, 130, 180);
	private static final String themeFont = "Arial";
	private static final String NO_DATA_AVAILABLE = "No data available";

	public enum ChartAspect {
		FLAT, THREE_D
	}

	private static class SectionColouriser {
		private int redDiff;
		private int greenDiff;
		private int blueDiff;

		private Color current;

		public Color getCurrent() {
			return current;
		}

		public SectionColouriser(Color baseColour, int columnCount) {
			this.current = baseColour;
			this.redDiff = (baseColour.getRed() / 2) / columnCount;
			this.greenDiff = (baseColour.getGreen() / 2) / columnCount;
			this.blueDiff = (baseColour.getBlue() / 2) / columnCount;
		}

		public void nextColour() {
			this.current = new Color(current.getRed() - redDiff, current.getGreen() - greenDiff, current.getBlue() - blueDiff);
		}

	}

	public static BufferedImage get3DBarChartImage(String sql, String domainTitle, String rangeTitle, int labelColumn, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, sql);
			JFreeChart chart = ChartFactory.createBarChart3D(EMPTY_STRING, domainTitle, rangeTitle, data, orientation, true, false, false);

			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			BarRenderer renderer = (BarRenderer) chart.getCategoryPlot().getRenderer();

			plot.setNoDataMessage(NO_DATA_AVAILABLE);
			plot.setOutlineVisible(false);

			// generate generic series colours
			SectionColouriser colouriser = new ThemeCharts.SectionColouriser(themeColour, plot.getDataset().getColumnCount());

			// set series renderers
			CategoryItemLabelGenerator generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), themeNumericFormat);
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				renderer.setSeriesItemLabelsVisible(seriesIndex, true);
				renderer.setSeriesPaint(seriesIndex, colouriser.getCurrent());
				colouriser.nextColour();
			}

			renderer.setItemLabelAnchorOffset(0);

			TextTitle title = chart.getTitle();
			title.setFont(new Font(themeFont, Font.BOLD, 12));

			Font axisFont = new Font(themeFont, Font.PLAIN, 12);
			plot.getDomainAxis().setLabelFont(axisFont);
			plot.getRangeAxis().setLabelFont(new Font(themeFont, Font.PLAIN, 14));

			chart.getLegend().setVisible(false);

			return chart.createBufferedImage(width, height);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
				connection = null;
			}
		}

		return null;
	}

	public static BufferedImage getLineChartImage(String sql, String domainTitle, String rangeTitle, int labelColumn, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, sql);
			JFreeChart chart = ChartFactory.createLineChart(EMPTY_STRING, domainTitle, rangeTitle, data, orientation, true, false, false);

			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.setBackgroundPaint(null);
			chart.getPlot().setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			// BarRenderer renderer = (BarRenderer) plot.getRenderer();
			LineAndShapeRenderer renderer = (LineAndShapeRenderer) chart.getCategoryPlot().getRenderer();

			plot.setNoDataMessage(NO_DATA_AVAILABLE);
			plot.setOutlineVisible(false);

			Color nextColour = themeColour;
			int redDiff = (themeColour.getRed() / 2) / plot.getDataset().getColumnCount();
			int greenDiff = (themeColour.getGreen() / 2) / plot.getDataset().getColumnCount();
			int blueDiff = (themeColour.getBlue() / 2) / plot.getDataset().getColumnCount();

			CategoryItemLabelGenerator generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), themeNumericFormat);
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				renderer.setSeriesItemLabelsVisible(seriesIndex, false);
				renderer.setSeriesPaint(seriesIndex, nextColour);
				nextColour = new Color(nextColour.getRed() - redDiff, nextColour.getGreen() - greenDiff, nextColour.getBlue() - blueDiff);
			}

			renderer.setItemLabelAnchorOffset(0);

			TextTitle title = chart.getTitle();
			title.setFont(new Font(themeFont, Font.BOLD, 12));

			Font axisFont = new Font(themeFont, Font.PLAIN, 12);
			plot.getDomainAxis().setLabelFont(axisFont);
			plot.getRangeAxis().setLabelFont(new Font(themeFont, Font.PLAIN, 14));

			chart.getLegend().setVisible(false);

			return chart.createBufferedImage(width, height);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
				connection = null;
			}
		}

		return null;
	}

	public static BufferedImage getAreaChartImage(String sql, String domainTitle, String rangeTitle, int labelColumn, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, sql);
			JFreeChart chart = ChartFactory.createAreaChart(EMPTY_STRING, domainTitle, rangeTitle, data, orientation, true, false, false);

			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.setBackgroundPaint(null);
			chart.getPlot().setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			// BarRenderer renderer = (BarRenderer) plot.getRenderer();
			AreaRenderer renderer = (AreaRenderer) chart.getCategoryPlot().getRenderer();

			plot.setNoDataMessage(NO_DATA_AVAILABLE);
			plot.setOutlineVisible(false);

			Color nextColour = themeColour;
			int redDiff = (themeColour.getRed() / 2) / plot.getDataset().getColumnCount();
			int greenDiff = (themeColour.getGreen() / 2) / plot.getDataset().getColumnCount();
			int blueDiff = (themeColour.getBlue() / 2) / plot.getDataset().getColumnCount();

			CategoryItemLabelGenerator generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), themeNumericFormat);
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				renderer.setSeriesItemLabelsVisible(seriesIndex, false);
				renderer.setSeriesPaint(seriesIndex, nextColour);
				nextColour = new Color(nextColour.getRed() - redDiff, nextColour.getGreen() - greenDiff, nextColour.getBlue() - blueDiff);
			}

			renderer.setItemLabelAnchorOffset(0);

			TextTitle title = chart.getTitle();
			title.setFont(new Font(themeFont, Font.BOLD, 12));

			Font axisFont = new Font(themeFont, Font.PLAIN, 12);
			plot.getDomainAxis().setLabelFont(axisFont);
			plot.getRangeAxis().setLabelFont(new Font(themeFont, Font.PLAIN, 14));

			chart.getLegend().setVisible(false);

			return chart.createBufferedImage(width, height);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
				connection = null;
			}
		}

		return null;
	}

	public static BufferedImage getPieChartImage(String sql, int labelColumn, int width, int height, ChartAspect aspect) throws Exception {
		Connection connection = null;
		try {

			connection = EXT.getPooledJDBCConnection();
			JDBCPieDataset data = new JDBCPieDataset(connection, sql);
			JFreeChart chart;
			if (ChartAspect.THREE_D.equals(aspect)) {
				chart = ChartFactory.createPieChart3D(EMPTY_STRING, data, true, false, false);
			} else {
				chart = ChartFactory.createPieChart(EMPTY_STRING, data, true, false, false);
			}
			chart.setBackgroundImageAlpha(0.0F);
			chart.getPlot().setBackgroundAlpha(0.0F);
			chart.setBackgroundPaint(null);

			PiePlot plot = (PiePlot) chart.getPlot();
			plot.setBackgroundAlpha(0.0F);
			plot.setNoDataMessage(NO_DATA_AVAILABLE);

			PieSectionLabelGenerator generator = new StandardPieSectionLabelGenerator(labelReference(labelColumn));
			plot.setLabelGenerator(generator); // null means no labels

			plot.setStartAngle(135);
			plot.setOutlineVisible(false);

			Color nextColour = themeColour;
			int redDiff = (themeColour.getRed() / 2) / plot.getDataset().getItemCount();
			int greenDiff = (themeColour.getGreen() / 2) / plot.getDataset().getItemCount();
			int blueDiff = (themeColour.getBlue() / 2) / plot.getDataset().getItemCount();

			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getItemCount(); seriesIndex++) {
				plot.setSectionPaint(plot.getDataset().getKey(seriesIndex), nextColour);
				nextColour = new Color(nextColour.getRed() - redDiff, nextColour.getGreen() - greenDiff, nextColour.getBlue() - blueDiff);
			}

			plot.setSectionOutlinesVisible(true);
			plot.setBaseSectionOutlinePaint(new Color(0xFFFFFF));
			plot.setBaseSectionOutlineStroke(new BasicStroke(2F));

			chart.removeLegend();

			return chart.createBufferedImage(width, height);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					e.printStackTrace();
				}
				connection = null;
			}
		}

		return null;
	}

	private static String labelReference(int columnIndex) {
		return "{" + columnIndex + "}";
	}
}
