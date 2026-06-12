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
import org.jfree.data.general.PieDataset;
import org.jfree.data.jdbc.JDBCCategoryDataset;
import org.jfree.data.jdbc.JDBCPieDataset;
import org.skyve.EXT;

import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

/**
 * Configures chart theming, palette colours, and rendering defaults used by admin reporting views.
 */
public class ThemeCharter {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(ThemeCharter.class);

	private static final String EMPTY_STRING = "";
	private static final DecimalFormat THEME_NUMERIC_FORMAT = new DecimalFormat("#,##0");
	private static final String THEME_FONT_NAME = "Arial";
	private static final int THEME_NORMAL_FONT_SIZE = 12;
	private static final int THEME_HEADER_FONT_SIZE = 14;

	private static final String NO_DATA_AVAILABLE = "No data available";
	private static final Color WHITE = new Color(255, 255, 255);
	private static final Color BLACK = new Color(0, 0, 0);
    
	private String sql;
	
	private String themeFontName;
	private int themeLegendFontSize;
	private int themeTitleFontSize;
	private int themeRangeFontSize;
	private int themeDomainFontSize;
	private SectionColouriser.Colouriser colouriserTheme;
	private Color themeBaseColour;
	private String[] colourPalette = {
			"#9898FF",
			"#EBC663",
			"#BD74F7",
			"#DEB1A0",
			"#82BDCE",
			"#65F8DF",
			"#A2C4A4",
			"#B76CBE",
			"#D6D879",
			"#EAA3FE",
			"#E3E3AD",
			"#EF7880",
			"#A8E5FF",
			"#86A9D8",
			"#94E76E",
			"#B779CC",
			"#DAAFA5",
			"#FFA6D2",
			"#E3E3AD"
	};
	
	/**
	 * Performs the getThemeBaseColour operation.
	 * @return the operation result
	 */
	public Color getThemeBaseColour() {
		return themeBaseColour;
	}

	/**
	 * Performs the setThemeBaseColour operation.
	 * @param themeBaseColour the themeBaseColour value
	 */
	public void setThemeBaseColour(Color themeBaseColour) {
		this.themeBaseColour = themeBaseColour;
	}
	
	/**
	 * Performs the getColourPalette operation.
	 * @return the operation result
	 */
	public String[] getColourPalette() {
		return colourPalette;
	}

	/**
	 * Performs the setColourPalette operation.
	 * @param colourPalette the colourPalette value
	 */
	public void setColourPalette(String[] colourPalette) {
		this.colourPalette = colourPalette;
	}

	/**
	 * Performs the getColouriserTheme operation.
	 * @return the operation result
	 */
	public SectionColouriser.Colouriser getColouriserTheme() {
		return colouriserTheme;
	}

	/**
	 * Performs the setColouriserTheme operation.
	 * @param colouriserTheme the colouriserTheme value
	 */
	public void setColouriserTheme(SectionColouriser.Colouriser colouriserTheme) {
		this.colouriserTheme = colouriserTheme;
	}

	/**
	 * Performs the getThemeLegendFontSize operation.
	 * @return the operation result
	 */
	public int getThemeLegendFontSize() {
		return themeLegendFontSize;
	}

	/**
	 * Performs the setThemeLegendFontSize operation.
	 * @param themeLegendFontSize the themeLegendFontSize value
	 */
	public void setThemeLegendFontSize(int themeLegendFontSize) {
		this.themeLegendFontSize = themeLegendFontSize;
	}

	/**
	 * Performs the getThemeTitleFontSize operation.
	 * @return the operation result
	 */
	public int getThemeTitleFontSize() {
		return themeTitleFontSize;
	}

	/**
	 * Performs the setThemeTitleFontSize operation.
	 * @param themeTitleFontSize the themeTitleFontSize value
	 */
	public void setThemeTitleFontSize(int themeTitleFontSize) {
		this.themeTitleFontSize = themeTitleFontSize;
	}

	/**
	 * Performs the getThemeRangeFontSize operation.
	 * @return the operation result
	 */
	public int getThemeRangeFontSize() {
		return themeRangeFontSize;
	}

	/**
	 * Performs the setThemeRangeFontSize operation.
	 * @param themeRangeFontSize the themeRangeFontSize value
	 */
	public void setThemeRangeFontSize(int themeRangeFontSize) {
		this.themeRangeFontSize = themeRangeFontSize;
	}

	/**
	 * Performs the getThemeDomainFontSize operation.
	 * @return the operation result
	 */
	public int getThemeDomainFontSize() {
		return themeDomainFontSize;
	}

	/**
	 * Performs the setThemeDomainFontSize operation.
	 * @param themeDomainFontSize the themeDomainFontSize value
	 */
	public void setThemeDomainFontSize(int themeDomainFontSize) {
		this.themeDomainFontSize = themeDomainFontSize;
	}

	/**
	 * Performs the getThemeLegendFont operation.
	 * @return the operation result
	 */
	public Font getThemeLegendFont() {
		return new Font(themeFontName, Font.PLAIN, themeLegendFontSize);
	}

	/**
	 * Performs the getThemeTitleFont operation.
	 * @return the operation result
	 */
	public Font getThemeTitleFont() {
		return new Font(themeFontName, Font.BOLD, themeTitleFontSize);
	}

	/**
	 * Performs the getThemeRangeFont operation.
	 * @return the operation result
	 */
	public Font getThemeRangeFont() {
		return new Font(themeFontName, Font.PLAIN, themeRangeFontSize);
	}

	/**
	 * Performs the getThemeDomainFont operation.
	 * @return the operation result
	 */
	public Font getThemeDomainFont() {
		return new Font(themeFontName, Font.PLAIN, themeDomainFontSize);
	}

	/**
	 * Performs the getThemeFontName operation.
	 * @return the operation result
	 */
	public String getThemeFontName() {
		return themeFontName;
	}

	/**
	 * Performs the setThemeFontName operation.
	 * @param themeFontName the themeFontName value
	 */
	public void setThemeFontName(String themeFontName) {
		this.themeFontName = themeFontName;
	}

	/**
	 * Performs the getSql operation.
	 * @return the operation result
	 */
	public String getSql() {
		return sql;
	}

	/**
	 * Performs the setSql operation.
	 * @param sql the sql value
	 */
	public void setSql(String sql) {
		this.sql = sql;
	}

	/**
	 * Creates a new ThemeCharter instance.
	 * @param themeColour the themeColour value
	 */
	public ThemeCharter(Color themeColour) {
		this.themeBaseColour = themeColour;
		themeFontName = THEME_FONT_NAME;
		themeLegendFontSize = THEME_HEADER_FONT_SIZE;
		themeTitleFontSize = THEME_HEADER_FONT_SIZE;
		themeRangeFontSize = THEME_NORMAL_FONT_SIZE;
		themeDomainFontSize = THEME_NORMAL_FONT_SIZE;
	}

	/**
	 * Creates a new ThemeCharter instance.
	 */
	public ThemeCharter() {
		this.themeBaseColour = new Color(70, 130, 180);
		themeFontName = THEME_FONT_NAME;
		themeLegendFontSize = THEME_HEADER_FONT_SIZE;
		themeTitleFontSize = THEME_HEADER_FONT_SIZE;
		themeRangeFontSize = THEME_NORMAL_FONT_SIZE;
		themeDomainFontSize = THEME_NORMAL_FONT_SIZE;
	}

	/**
	 * Derives sequential colours for chart series from either a gradient or a palette.
	 */
	public static class SectionColouriser {
		/**
		 * Defines the colour derivation strategy for subsequent series colours.
		 */
		public enum Colouriser {
			SINGLE_COLOUR, MULTI_COLOUR
		}
		
		private int redDiff;
		private int greenDiff;
		private int blueDiff;
		private Colouriser colouriser = Colouriser.SINGLE_COLOUR;
		private int currentColourIndex = 0;
		
		private Color current;
		private String[] palette;
		
		/**
		 * Returns the active colour derivation strategy.
		 *
		 * @return The configured colour derivation strategy.
		 */
		public Colouriser getColouriser() {
			return colouriser;
		}

		/**
		 * Sets the colour derivation strategy.
		 *
		 * @param colouriser The strategy to apply for future colour generation.
		 */
		public void setColour(Colouriser colouriser) {
			this.colouriser = colouriser;
		}

		/**
		 * Returns the currently selected series colour.
		 *
		 * @return The current series colour.
		 */
		public Color getCurrent() {
			return current;
		}

		/**
		 * Sets the currently selected series colour.
		 *
		 * @param current The colour to use as the current series colour.
		 */
		public void setCurrent(Color current) {
			this.current = current;
		}

		/**
		 * Returns the red-channel decrement applied for single-colour gradients.
		 *
		 * @return The red-channel decrement value.
		 */
		public int getRedDiff() {
			return redDiff;
		}

		/**
		 * Sets the red-channel decrement applied for single-colour gradients.
		 *
		 * @param redDiff The red-channel decrement value.
		 */
		public void setRedDiff(int redDiff) {
			this.redDiff = redDiff;
		}

		/**
		 * Returns the green-channel decrement applied for single-colour gradients.
		 *
		 * @return The green-channel decrement value.
		 */
		public int getGreenDiff() {
			return greenDiff;
		}

		/**
		 * Sets the green-channel decrement applied for single-colour gradients.
		 *
		 * @param greenDiff The green-channel decrement value.
		 */
		public void setGreenDiff(int greenDiff) {
			this.greenDiff = greenDiff;
		}

		/**
		 * Returns the blue-channel decrement applied for single-colour gradients.
		 *
		 * @return The blue-channel decrement value.
		 */
		public int getBlueDiff() {
			return blueDiff;
		}

		/**
		 * Sets the blue-channel decrement applied for single-colour gradients.
		 *
		 * @param blueDiff The blue-channel decrement value.
		 */
		public void setBlueDiff(int blueDiff) {
			this.blueDiff = blueDiff;
		}
		
		/**
		 * Creates a colour sequencer for chart sections.
		 *
		 * @param baseColour The base colour used for gradient derivation.
		 * @param columnCount The number of series/columns to colour.
		 * @param colouriser The colour derivation strategy.
		 * @param palette The palette to use when {@code colouriser} is multi-colour.
		 */
		public SectionColouriser(Color baseColour, int columnCount, SectionColouriser.Colouriser colouriser, String[] palette) {
			this.current = baseColour;
			this.colouriser = colouriser;
			this.palette = palette;
			if (columnCount != 0) {
				this.redDiff = (baseColour.getRed() / 2) / columnCount;
				this.greenDiff = (baseColour.getGreen() / 2) / columnCount;
				this.blueDiff = (baseColour.getBlue() / 2) / columnCount;
			}
		}

		/**
		 * Advances to the next colour in the configured sequence.
		 */
		public void nextColour() {
			if(colouriser==Colouriser.MULTI_COLOUR) {
				this.current = Color.decode(palette[currentColourIndex++]);
			} else {
				this.current = new Color(current.getRed() - redDiff, current.getGreen() - greenDiff,
					current.getBlue() - blueDiff);
			}
		}
	}

	/**
	 * Performs the getBarChartImage operation.
	 * @param domainTitle the domainTitle value
	 * @param rangeTitle the rangeTitle value
	 * @param labelColumn the labelColumn value
	 * @param orientation the orientation value
	 * @param width the width value
	 * @param height the height value
	 * @param showLegend the showLegend value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	public BufferedImage getBarChartImage(String domainTitle, String rangeTitle, Integer labelColumn,
			PlotOrientation orientation, int width, int height, boolean showLegend)
			throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getDataStoreConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, this.sql);

			JFreeChart chart = ChartFactory.createBarChart(EMPTY_STRING, domainTitle, rangeTitle, data, orientation, true,
						false, false);
			chart.setBackgroundImageAlpha(0.8F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.getPlot().setBackgroundPaint(null);
			chart.setBackgroundPaint(null);

			CategoryPlot plot = (CategoryPlot) chart.getPlot();
			plot.setRangeGridlinesVisible(false);

			BarRenderer renderer = (BarRenderer) plot.getRenderer();
			renderer.setBarPainter(new StandardBarPainter());

			plot.setNoDataMessage(NO_DATA_AVAILABLE);
			plot.setOutlineVisible(false);

			// generate generic series colours
			SectionColouriser colouriser = new SectionColouriser(this.themeBaseColour, plot.getDataset().getColumnCount(), getColouriserTheme(), getColourPalette());

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
			title.setFont(getThemeTitleFont());

			plot.getDomainAxis().setLabelFont(getThemeDomainFont());
			plot.getRangeAxis().setLabelFont(getThemeRangeFont());

			chart.getLegend().setItemFont(getThemeLegendFont());
			chart.getLegend().setVisible(showLegend);

			return chart.createBufferedImage(width, height);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					LOGGER.error(e.getMessage(), e);
				}
				connection = null;
			}
		}

		return null;
	}

	/**
	 * Performs the getLineChartImage operation.
	 * @param domainTitle the domainTitle value
	 * @param rangeTitle the rangeTitle value
	 * @param labelColumn the labelColumn value
	 * @param orientation the orientation value
	 * @param width the width value
	 * @param height the height value
	 * @param showLegend the showLegend value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	public BufferedImage getLineChartImage(String domainTitle, String rangeTitle, Integer labelColumn,
			PlotOrientation orientation, int width, int height, boolean showLegend) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getDataStoreConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, this.sql);
			JFreeChart chart = ChartFactory.createLineChart(EMPTY_STRING, domainTitle, rangeTitle, data, orientation,
					true, false, false);

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

			SectionColouriser colouriser = new SectionColouriser(this.themeBaseColour, plot.getDataset().getColumnCount(), getColouriserTheme(), getColourPalette());

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
			title.setFont(getThemeTitleFont());

			plot.getDomainAxis().setLabelFont(getThemeDomainFont());
			plot.getRangeAxis().setLabelFont(getThemeRangeFont());

			chart.getLegend().setItemFont(getThemeLegendFont());
			chart.getLegend().setVisible(showLegend);

			return chart.createBufferedImage(width, height);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					LOGGER.error(e.getMessage(), e);
				}
				connection = null;
			}
		}

		return null;
	}

	/**
	 * Performs the getAreaChartImage operation.
	 * @param domainTitle the domainTitle value
	 * @param rangeTitle the rangeTitle value
	 * @param labelColumn the labelColumn value
	 * @param orientation the orientation value
	 * @param width the width value
	 * @param height the height value
	 * @param showLegend the showLegend value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	public BufferedImage getAreaChartImage(String domainTitle, String rangeTitle, Integer labelColumn,
			PlotOrientation orientation, int width, int height, boolean showLegend) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getDataStoreConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, this.sql);
			JFreeChart chart = ChartFactory.createAreaChart(EMPTY_STRING, domainTitle, rangeTitle, data, orientation,
					true, false, false);

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

			SectionColouriser colouriser = new SectionColouriser(this.themeBaseColour, plot.getDataset().getColumnCount(), getColouriserTheme(), getColourPalette());

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
			title.setFont(getThemeTitleFont());

			plot.getDomainAxis().setLabelFont(getThemeDomainFont());
			plot.getRangeAxis().setLabelFont(getThemeRangeFont());

			chart.getLegend().setItemFont(getThemeLegendFont());
			chart.getLegend().setVisible(showLegend);

			return chart.createBufferedImage(width, height);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					LOGGER.error(e.getMessage(), e);
				}
				connection = null;
			}
		}

		return null;
	}

	/**
	 * Performs the getPieChartImage operation.
	 * @param title the title value
	 * @param labelColumn the labelColumn value
	 * @param width the width value
	 * @param height the height value
	 * @param showLegend the showLegend value
	 * @param data the data value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	public BufferedImage getPieChartImage(String title, Integer labelColumn, int width, int height,
			boolean showLegend, PieDataset data) throws Exception {

		JFreeChart chart = ChartFactory.createPieChart(title, data, true, false, false);
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

		SectionColouriser colouriser = new SectionColouriser(this.themeBaseColour, plot.getDataset().getItemCount(), getColouriserTheme(), getColourPalette());

		for (int seriesIndex = 0; seriesIndex < plot.getDataset().getItemCount(); seriesIndex++) {
			plot.setSectionPaint(plot.getDataset().getKey(seriesIndex), colouriser.getCurrent());
			colouriser.nextColour();
		}

		TextTitle textTitle = chart.getTitle();
		textTitle.setFont(getThemeTitleFont());

		plot.setSectionOutlinesVisible(true);
		plot.setDefaultSectionOutlinePaint(new Color(0xFFFFFF));
		plot.setDefaultSectionOutlineStroke(new BasicStroke(2F));

		if (!showLegend) {
			chart.removeLegend();
		} else {
			chart.getLegend().setItemFont(getThemeLegendFont());
		}

		return chart.createBufferedImage(width, height);
	}

	/**
	 * Performs the getPieChartImage operation.
	 * @param title the title value
	 * @param labelColumn the labelColumn value
	 * @param width the width value
	 * @param height the height value
	 * @param showLegend the showLegend value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	public BufferedImage getPieChartImage(String title, Integer labelColumn, int width, int height, boolean showLegend)
			throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getDataStoreConnection();
			JDBCPieDataset data = new JDBCPieDataset(connection, this.sql);
			return getPieChartImage(title, labelColumn, width, height, showLegend, data);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		} finally {
			if (connection != null) {
				try {
					connection.close();
				} catch (SQLException e) {
					LOGGER.error(e.getMessage(), e);
				}
				connection = null;
			}
		}

		return null;
	}
	
	/**
	 * Builds a chart-label formatter token reference for the supplied column index.
	 *
	 * @param columnIndex The column index to reference.
	 * @return A formatter token such as {@code {0}}.
	 */
	private static String labelReference(Integer columnIndex) {
		return "{" + columnIndex.toString() + "}";
	}

	/**
	 * A Fabulator is an ordered coloured list of string-bigint combinations,
	 * typically to represent comparative totals
	 * 
	 * List<Object[]> objects is the tupleResults() returned from SQL similar to
	 * "select name, count(*) from a group by name"
	 * 
	 * @param width
	 * @param height
	 * @param objects
	 * @param focusString
	 * @return
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public BufferedImage getFabulator(int width, int height, List<Object[]> objects, String focusString) {
		BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

		Graphics graphics = img.createGraphics();
		Color backCol = WHITE;
		graphics.setColor(backCol);
		graphics.fillRect(0, 0, width, height);

		Font font = getThemeTitleFont();
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

		SectionColouriser colouriser = new SectionColouriser(this.themeBaseColour, rows, getColouriserTheme(), getColourPalette());
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
