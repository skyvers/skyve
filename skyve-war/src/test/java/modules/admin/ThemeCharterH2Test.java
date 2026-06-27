package modules.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.awt.Color;
import java.awt.image.BufferedImage;

import org.jfree.chart.plot.PlotOrientation;
import org.junit.jupiter.api.Test;

import modules.admin.ThemeCharter.SectionColouriser;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ThemeCharterH2Test extends AbstractH2Test {
	private static final String CATEGORY_SQL = "select 'Alpha' as category, 10 as first_value, 20 as second_value";
	private static final String PIE_SQL = "select 'Alpha' as section_name, 10 as section_value";

	@Test
	void getBarChartImageRendersJdbcCategoryDatasetWithLabels() throws Exception {
		ThemeCharter charter = themedCharter(CATEGORY_SQL);

		BufferedImage image = charter.getBarChartImage("Category", "Value", Integer.valueOf(1),
				PlotOrientation.VERTICAL, 240, 160, true);

		assertNotNull(image);
		assertEquals(240, image.getWidth());
		assertEquals(160, image.getHeight());
	}

	@Test
	void getLineChartImageRendersJdbcCategoryDatasetWithoutLabels() throws Exception {
		ThemeCharter charter = themedCharter(CATEGORY_SQL);

		BufferedImage image = charter.getLineChartImage("Category", "Value", null,
				PlotOrientation.HORIZONTAL, 220, 140, false);

		assertNotNull(image);
		assertEquals(220, image.getWidth());
		assertEquals(140, image.getHeight());
	}

	@Test
	void getAreaChartImageRendersJdbcCategoryDatasetWithMultiColourPalette() throws Exception {
		ThemeCharter charter = themedCharter(CATEGORY_SQL);
		charter.setColouriserTheme(SectionColouriser.Colouriser.MULTI_COLOUR);
		charter.setColourPalette(new String[] { "#FF0000", "#00FF00", "#0000FF" });

		BufferedImage image = charter.getAreaChartImage("Category", "Value", Integer.valueOf(1),
				PlotOrientation.VERTICAL, 200, 120, true);

		assertNotNull(image);
		assertEquals(200, image.getWidth());
		assertEquals(120, image.getHeight());
	}

	@Test
	void getPieChartImageRendersJdbcPieDataset() throws Exception {
		ThemeCharter charter = themedCharter(PIE_SQL);

		BufferedImage image = charter.getPieChartImage("Pie", Integer.valueOf(0), 180, 130, true);

		assertNotNull(image);
		assertEquals(180, image.getWidth());
		assertEquals(130, image.getHeight());
	}

	@Test
	void getChartImageReturnsNullForInvalidSql() throws Exception {
		ThemeCharter charter = themedCharter("select * from definitely_missing_chart_table");

		BufferedImage image = charter.getLineChartImage("Category", "Value", null,
				PlotOrientation.VERTICAL, 100, 80, false);

		assertNull(image);
	}

	private static ThemeCharter themedCharter(String sql) {
		ThemeCharter charter = new ThemeCharter(new Color(80, 120, 160));
		charter.setSql(sql);
		return charter;
	}
}
