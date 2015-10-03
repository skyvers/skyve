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

	public enum ChartAspect {
		FLAT, THREE_D
	}

	public static BufferedImage get3DBarChartImage(String sql, String domainTitle, String rangeTitle, int itemLabelColumnIndex, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, sql);
			JFreeChart chart = ChartFactory.createBarChart3D("", domainTitle, rangeTitle, data, orientation, true, false, false);

			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			BarRenderer renderer = (BarRenderer) chart.getCategoryPlot().getRenderer();

			plot.setNoDataMessage("No data available");
			plot.setOutlineVisible(false);

			// generate generic series colours
			Color baseColour = new Color(70, 130, 180);
			Color nextColour = baseColour;
			int redDiff = (baseColour.getRed() / 2) / plot.getDataset().getColumnCount();
			int greenDiff = (baseColour.getGreen() / 2) / plot.getDataset().getColumnCount();
			int blueDiff = (baseColour.getBlue() / 2) / plot.getDataset().getColumnCount();

			// set series renderers
			String colIndex = "{" + itemLabelColumnIndex + "}";
			CategoryItemLabelGenerator generator = new StandardCategoryItemLabelGenerator(colIndex, new DecimalFormat("#,##0"));
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				renderer.setSeriesItemLabelsVisible(seriesIndex, true);
				renderer.setSeriesPaint(seriesIndex, nextColour);
				nextColour = new Color(nextColour.getRed() - redDiff, nextColour.getGreen() - greenDiff, nextColour.getBlue() - blueDiff);
			}

			renderer.setItemLabelAnchorOffset(0);

			TextTitle title = chart.getTitle();
			title.setFont(new Font("Arial Unicode MS", Font.BOLD, 12));

			Font axisFont = new Font("Arial", Font.PLAIN, 12);
			plot.getDomainAxis().setLabelFont(axisFont);
			plot.getRangeAxis().setLabelFont(new Font("Arial", Font.PLAIN, 14));

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

	public static BufferedImage getLineChartImage(String sql, String domainTitle, String rangeTitle, int itemLabelColumnIndex, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, sql);
			JFreeChart chart = ChartFactory.createLineChart("", domainTitle, rangeTitle, data, orientation, true, false, false);

			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.setBackgroundPaint(null);
			chart.getPlot().setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			// BarRenderer renderer = (BarRenderer) plot.getRenderer();
			LineAndShapeRenderer renderer = (LineAndShapeRenderer) chart.getCategoryPlot().getRenderer();

			plot.setNoDataMessage("No data available");
			plot.setOutlineVisible(false);

			Color baseColour = new Color(70, 130, 180);
			Color nextColour = baseColour;
			int redDiff = (baseColour.getRed() / 2) / plot.getDataset().getColumnCount();
			int greenDiff = (baseColour.getGreen() / 2) / plot.getDataset().getColumnCount();
			int blueDiff = (baseColour.getBlue() / 2) / plot.getDataset().getColumnCount();

			String colIndex = "{" + itemLabelColumnIndex + "}";
			CategoryItemLabelGenerator generator = new StandardCategoryItemLabelGenerator(colIndex, new DecimalFormat("#,##0"));
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				renderer.setSeriesItemLabelsVisible(seriesIndex, false);
				renderer.setSeriesPaint(seriesIndex, nextColour);
				nextColour = new Color(nextColour.getRed() - redDiff, nextColour.getGreen() - greenDiff, nextColour.getBlue() - blueDiff);
			}

			renderer.setItemLabelAnchorOffset(0);
			// renderer.setBaseItemLabelFont(new Font("Arial", 0, 10));

			TextTitle title = chart.getTitle();
			title.setFont(new Font("Arial Unicode MS", Font.BOLD, 12));

			Font axisFont = new Font("Arial", Font.PLAIN, 12);
			plot.getDomainAxis().setLabelFont(axisFont);
			plot.getRangeAxis().setLabelFont(new Font("Arial", Font.PLAIN, 14));

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

	public static BufferedImage getAreaChartImage(String sql, String domainTitle, String rangeTitle, int itemLabelColumnIndex, PlotOrientation orientation, int width, int height) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, sql);
			JFreeChart chart = ChartFactory.createAreaChart("", domainTitle, rangeTitle, data, orientation, true, false, false);

			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.setBackgroundPaint(null);
			chart.getPlot().setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			// BarRenderer renderer = (BarRenderer) plot.getRenderer();
			AreaRenderer renderer = (AreaRenderer) chart.getCategoryPlot().getRenderer();

			plot.setNoDataMessage("No data available");
			plot.setOutlineVisible(false);

			Color baseColour = new Color(70, 130, 180);
			Color nextColour = baseColour;
			int redDiff = (baseColour.getRed() / 2) / plot.getDataset().getColumnCount();
			int greenDiff = (baseColour.getGreen() / 2) / plot.getDataset().getColumnCount();
			int blueDiff = (baseColour.getBlue() / 2) / plot.getDataset().getColumnCount();

			String colIndex = "{" + itemLabelColumnIndex + "}";
			CategoryItemLabelGenerator generator = new StandardCategoryItemLabelGenerator(colIndex, new DecimalFormat("#,##0"));
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				renderer.setSeriesItemLabelsVisible(seriesIndex, false);
				renderer.setSeriesPaint(seriesIndex, nextColour);
				nextColour = new Color(nextColour.getRed() - redDiff, nextColour.getGreen() - greenDiff, nextColour.getBlue() - blueDiff);
			}

			renderer.setItemLabelAnchorOffset(0);
			// renderer.setBaseItemLabelFont(new Font("Arial", 0, 10));

			TextTitle title = chart.getTitle();
			title.setFont(new Font("Arial Unicode MS", Font.BOLD, 12));

			Font axisFont = new Font("Arial", Font.PLAIN, 12);
			plot.getDomainAxis().setLabelFont(axisFont);
			plot.getRangeAxis().setLabelFont(new Font("Arial", Font.PLAIN, 14));

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

	public static BufferedImage getPieChartImage(String sql, int itemLabelColumnIndex, int width, int height, ChartAspect aspect) throws Exception {
		Connection connection = null;
		try {

			connection = EXT.getPooledJDBCConnection();
			JDBCPieDataset data = new JDBCPieDataset(connection, sql);
			JFreeChart chart;
			if (ChartAspect.THREE_D.equals(aspect)) {
				chart = ChartFactory.createPieChart3D("", data, true, false, false);
			} else {
				chart = ChartFactory.createPieChart("", data, true, false, false);
			}
			chart.setBackgroundImageAlpha(0.0F);
			chart.getPlot().setBackgroundAlpha(0.0F);
			chart.setBackgroundPaint(null);

			PiePlot plot = (PiePlot) chart.getPlot();
			plot.setBackgroundAlpha(0.0F);
			plot.setNoDataMessage("No data available");

			String colIndex = "{" + itemLabelColumnIndex + "}";
			PieSectionLabelGenerator generator = new StandardPieSectionLabelGenerator(colIndex);
			plot.setLabelGenerator(generator); // null means no labels

			plot.setStartAngle(135);
			plot.setOutlineVisible(false);

			Color baseColour = new Color(70, 130, 180);
			Color nextColour = baseColour;
			int redDiff = (baseColour.getRed() / 2) / plot.getDataset().getItemCount();
			int greenDiff = (baseColour.getGreen() / 2) / plot.getDataset().getItemCount();
			int blueDiff = (baseColour.getBlue() / 2) / plot.getDataset().getItemCount();

			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getItemCount(); seriesIndex++) {
				plot.setSectionPaint(plot.getDataset().getKey(seriesIndex), nextColour);
				nextColour = new Color(nextColour.getRed() - redDiff, nextColour.getGreen() - greenDiff, nextColour.getBlue() - blueDiff);
			}

			plot.setLabelFont(new Font("Arial", 0, 9));
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
}
