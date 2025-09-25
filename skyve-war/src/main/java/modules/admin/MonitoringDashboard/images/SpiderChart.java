package modules.admin.MonitoringDashboard.images;

import java.awt.image.BufferedImage;

import org.jfree.chart.plot.PlotOrientation;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

import modules.admin.ThemeCharter;
import modules.admin.domain.MonitoringDashboard;

/**
 * Dynamic image implementation for testing dynamic image servlet requests.
 * 
 * This class generates a test chart using synthetic data to verify that the
 * dynamic image functionality is working correctly. It's primarily used for
 * testing the /dynamic.png endpoint and monitoring system.
 * 
 * The generated chart displays sample request type performance data with
 * multiple data series to demonstrate chart rendering capabilities.
 */
public class SpiderChart implements DynamicImage<MonitoringDashboard> {
	private static final long serialVersionUID = -7167907371272427864L;

	@Override
	public ImageFormat getFormat() {
		return null;
	}

	@Override
	public Float getCompressionQuality() {
		return null;
	}

	@Override
	public BufferedImage getImage(MonitoringDashboard bean, int width, int height, User user) throws Exception {
		ThemeCharter charter = new ThemeCharter();
		// Test SQL string for dynamic image testing
		String testSql = "SELECT " +
				"'Create' as category, 5 as value1, 3 as value2 UNION ALL " +
				"SELECT 'Edit' as category, 8 as value1, 6 as value2 UNION ALL " +
				"SELECT 'Query' as category, 12 as value1, 9 as value2 UNION ALL " +
				"SELECT 'SmartClient' as category, 15 as value1, 11 as value2 UNION ALL " +
				"SELECT 'Reports' as category, 7 as value1, 4 as value2";
		charter.setSql(testSql);
		return charter.getAreaChartImage("Request Types Performance", "Operations", null, PlotOrientation.VERTICAL, width, height,
				false);
	}
}
