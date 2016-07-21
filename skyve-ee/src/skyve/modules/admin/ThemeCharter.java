package modules.admin;

import java.awt.BasicStroke;
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.math.BigInteger;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.DecimalFormat;
import java.util.List;

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

	private static final String EMPTY_STRING = "";
	private static final DecimalFormat THEME_NUMERIC_FORMAT = new DecimalFormat("#,##0");
	private static final String THEME_FONT_NAME = "Arial";
	private static final int THEME_NORMAL_FONT_SIZE = 12;
	private static final int THEME_HEADER_FONT_SIZE = 14;
	private static final Font THEME_LEGEND_FONT = new Font(THEME_FONT_NAME, Font.PLAIN, THEME_NORMAL_FONT_SIZE);
	private static final Font THEME_TITLE_FONT = new Font(THEME_FONT_NAME, Font.BOLD, THEME_NORMAL_FONT_SIZE);
	private static final Font THEME_RANGE_FONT = new Font(THEME_FONT_NAME, Font.PLAIN, THEME_HEADER_FONT_SIZE);
	private static final Font THEME_DOMAIN_FONT = new Font(THEME_FONT_NAME, Font.PLAIN, THEME_NORMAL_FONT_SIZE);
	
	private static final String NO_DATA_AVAILABLE = "No data available";
	private static final Color WHITE = new Color(255, 255, 255);
	private static final Color BLACK = new Color(0, 0, 0);
	
	
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

	public BufferedImage getBarChartImage(String domainTitle, String rangeTitle, Integer labelColumn, PlotOrientation orientation, int width, int height, ChartAspect aspect, boolean showLegend) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getDataStoreConnection();
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
				generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), THEME_NUMERIC_FORMAT);
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
			title.setFont(THEME_TITLE_FONT);

			plot.getDomainAxis().setLabelFont(THEME_DOMAIN_FONT);
			plot.getRangeAxis().setLabelFont(THEME_RANGE_FONT);

			chart.getLegend().setItemFont(THEME_LEGEND_FONT);
			chart.getLegend().setVisible(showLegend);

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

	public BufferedImage getLineChartImage(String domainTitle, String rangeTitle, Integer labelColumn, PlotOrientation orientation, int width, int height, boolean showLegend) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getDataStoreConnection();
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
				generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), THEME_NUMERIC_FORMAT);
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
			title.setFont(THEME_TITLE_FONT);

			plot.getDomainAxis().setLabelFont(THEME_DOMAIN_FONT);
			plot.getRangeAxis().setLabelFont(THEME_RANGE_FONT);

			chart.getLegend().setItemFont(THEME_LEGEND_FONT);
			chart.getLegend().setVisible(showLegend);

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

	public BufferedImage getAreaChartImage(String domainTitle, String rangeTitle, Integer labelColumn, PlotOrientation orientation, int width, int height, boolean showLegend) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getDataStoreConnection();
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
				generator = new StandardCategoryItemLabelGenerator(labelReference(labelColumn), THEME_NUMERIC_FORMAT);
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
			title.setFont(THEME_TITLE_FONT);

			plot.getDomainAxis().setLabelFont(THEME_DOMAIN_FONT);
			plot.getRangeAxis().setLabelFont(THEME_RANGE_FONT);

			chart.getLegend().setItemFont(THEME_LEGEND_FONT);
			chart.getLegend().setVisible(showLegend);

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

	public BufferedImage getPieChartImage(String title, Integer labelColumn, int width, int height, ChartAspect aspect, boolean showLegend) throws Exception {
		Connection connection = null;
		try {

			connection = EXT.getDataStoreConnection();
			JDBCPieDataset data = new JDBCPieDataset(connection, this.sql);
			JFreeChart chart;
			if (ChartAspect.THREE_D.equals(aspect)) {
				chart = ChartFactory.createPieChart3D(title, data, true, false, false);
			} else {
				chart = ChartFactory.createPieChart(title, data, true, false, false);
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
			
			TextTitle textTitle = chart.getTitle();
			textTitle.setFont(THEME_TITLE_FONT);

			plot.setSectionOutlinesVisible(true);
			plot.setBaseSectionOutlinePaint(new Color(0xFFFFFF));
			plot.setBaseSectionOutlineStroke(new BasicStroke(2F));

			if (!showLegend) {
				chart.removeLegend();
			} else {
				chart.getLegend().setItemFont(THEME_LEGEND_FONT);				
			}

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
	
	/**
	 * A Fabulator is an ordered coloured list of string-bigint combinations, typically to represent comparative totals
	 * 
	 * List<Object[]> objects is the tupleResults() returned from SQL similar to
	 * 	"select name, count(*) from a group by name"
	 * 
	 * @param width
	 * @param height
	 * @param objects
	 * @param focusString
	 * @return
	 */
	public BufferedImage getFabulator(int width, int height, List<Object[]> objects, String focusString) {
		BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

		Graphics graphics = img.createGraphics();
		Color backCol = WHITE;
		graphics.setColor(backCol);
		graphics.fillRect(0, 0, width, height);

		Font font = THEME_TITLE_FONT;
		Canvas c = new Canvas();
		FontMetrics fm = c.getFontMetrics(font);
		graphics.setFont(font);

		boolean focusFound = false;
		if (focusString == null) {
			focusFound = true;
		}

		int xPad = 5;
		int yPad = 2;
		int topNumber = height / (fm.getHeight() + (2 * yPad));
		if (topNumber == 0) {
			topNumber = 1;
		}
		int rows = 0;
		if (!objects.isEmpty()) {
			rows = objects.size();
		}
		if (rows < topNumber) {
			topNumber = rows;
		}
		if (topNumber == 0) {
			topNumber = 1;
		}

		SectionColouriser colouriser = new SectionColouriser(this.themeColour, rows);
		int counter = 0;

		for (Object[] o : objects) {

			// put in the string
			Object[] values = o;
			String label = (String) values[0];
			BigInteger val = (BigInteger) values[1];

			if (label.equals(focusString)) {
				focusFound = true;
			}

			// draw thing if this is the focus, or the focus has been previously
			// found, or there is still space for the focus
			if (label.equals(focusString) || focusFound || (!focusFound && counter < topNumber - 1)) {
				// create each object as a concatenated string drawn on top of a
				// colour bar
				Color barCol = colouriser.getCurrent();
				if (label.equals(focusString)) {
					barCol = BLACK;
				}
				graphics.setColor(barCol);
				int x = xPad;
				int y = (fm.getHeight() + (yPad * 2)) * counter;
				graphics.fillRect(0, y, width, fm.getHeight() + (yPad * 2));

				StringBuilder sb = new StringBuilder();
				if (val == null) {
					sb.append(" ").append(0);
				} else {
					sb.append(val.toString());
				}
				sb.append(" (").append(label).append(")");

				graphics.setColor(WHITE);
				if (label.equals(focusString)) {
					graphics.setColor(WHITE);
				}
				graphics.drawString(sb.toString(), x, y + fm.getHeight() - yPad);

				// inc colour
				colouriser.nextColour();
				counter++;
			}

			if (counter > topNumber - 1) {
				break;
			}
		}

		return img;
	}

}
