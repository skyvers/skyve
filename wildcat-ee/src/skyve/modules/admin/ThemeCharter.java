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
import org.jfree.chart.renderer.category.StandardBarPainter;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.jdbc.JDBCCategoryDataset;
import org.jfree.data.jdbc.JDBCPieDataset;
import org.skyve.EXT;

public class ThemeCharter {

	private static final DecimalFormat themeNumericFormat = new DecimalFormat("#,##0");
	private static final String EMPTY_STRING = "";
	private static final String themeFont = "Arial";
	private static final String NO_DATA_AVAILABLE = "No data available";

	private String sql;
	private Color themeColour;

	public String getSql() {
		return sql;
	}

	public void setSql(String sql) {
		this.sql = sql;
	}

	public Color getThemeColour() {
		return themeColour;
	}

	public void setThemeColour(Color themeColour) {
		this.themeColour = themeColour;
	}

	public enum ChartAspect {
		FLAT, THREE_D
	}

	public ThemeCharter(Color themeColour) {
		this.themeColour = themeColour;
	}

	public ThemeCharter() {
		this.themeColour = new Color(70, 130, 180);
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
			if (columnCount != 0) {
				this.redDiff = (baseColour.getRed() / 2) / columnCount;
				this.greenDiff = (baseColour.getGreen() / 2) / columnCount;
				this.blueDiff = (baseColour.getBlue() / 2) / columnCount;
			}
		}

		public void nextColour() {
			this.current = new Color(current.getRed() - redDiff, current.getGreen() - greenDiff, current.getBlue() - blueDiff);
		}

	}

	public BufferedImage getBarChartImage(String domainTitle, String rangeTitle, Integer labelColumn, PlotOrientation orientation, int width, int height, ChartAspect aspect) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, this.sql);

			JFreeChart chart;
			if (ChartAspect.THREE_D.equals(aspect)) {
				chart = ChartFactory.createBarChart3D(EMPTY_STRING, domainTitle, rangeTitle, data, orientation, true, false, false);
			} else {
				chart = ChartFactory.createBarChart(EMPTY_STRING, domainTitle, rangeTitle, data, orientation, true, false, false);
			}
			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.getPlot().setBackgroundPaint(null);
			chart.setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			BarRenderer renderer;
			if (ChartAspect.THREE_D.equals(aspect)) {
				renderer = (BarRenderer) chart.getCategoryPlot().getRenderer();
			} else {
				renderer = (BarRenderer) plot.getRenderer();
				renderer.setBarPainter(new StandardBarPainter());
			}

			plot.setNoDataMessage(NO_DATA_AVAILABLE);
			plot.setOutlineVisible(false);

			// generate generic series colours
			SectionColouriser colouriser = new ThemeCharter.SectionColouriser(this.themeColour, plot.getDataset().getColumnCount());

			// set series renderers
			CategoryItemLabelGenerator generator = null;
			if (labelColumn != null) {
				generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), themeNumericFormat);
			}
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				if (labelColumn != null) {
					renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				}
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

	public BufferedImage getLineChartImage(String domainTitle, String rangeTitle, Integer labelColumn, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, this.sql);
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

			SectionColouriser colouriser = new ThemeCharter.SectionColouriser(themeColour, plot.getDataset().getColumnCount());

			CategoryItemLabelGenerator generator = null;
			if (labelColumn != null) {
				generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), themeNumericFormat);
			}
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				if (labelColumn != null) {
					renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				}
				renderer.setSeriesItemLabelsVisible(seriesIndex, false);
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

	public BufferedImage getAreaChartImage(String domainTitle, String rangeTitle, Integer labelColumn, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, this.sql);
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

			SectionColouriser colouriser = new ThemeCharter.SectionColouriser(themeColour, plot.getDataset().getColumnCount());

			CategoryItemLabelGenerator generator = null;
			if (labelColumn != null) {
				generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), themeNumericFormat);
			}
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				if (labelColumn != null) {
					renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				}
				renderer.setSeriesItemLabelsVisible(seriesIndex, false);
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

	public BufferedImage getPieChartImage(Integer labelColumn, int width, int height, ChartAspect aspect) throws Exception {
		Connection connection = null;
		try {

			connection = EXT.getPooledJDBCConnection();
			JDBCPieDataset data = new JDBCPieDataset(connection, this.sql);
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

			if (labelColumn != null) {
				PieSectionLabelGenerator generator = new StandardPieSectionLabelGenerator(labelReference(labelColumn));
				plot.setLabelGenerator(generator); // null means no labels
			}

			plot.setStartAngle(135);
			plot.setOutlineVisible(false);

			SectionColouriser colouriser = new ThemeCharter.SectionColouriser(themeColour, plot.getDataset().getItemCount());

			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getItemCount(); seriesIndex++) {
				plot.setSectionPaint(plot.getDataset().getKey(seriesIndex), colouriser.getCurrent());
				colouriser.nextColour();
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

	private static String labelReference(Integer columnIndex) {
		return "{" + columnIndex.toString() + "}";
	}
}
