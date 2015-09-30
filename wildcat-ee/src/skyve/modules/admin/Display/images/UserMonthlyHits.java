package modules.admin.Display.images;

import java.awt.Color;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.DecimalFormat;

import modules.admin.domain.Display;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.labels.CategoryItemLabelGenerator;
import org.jfree.chart.labels.StandardCategoryItemLabelGenerator;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.data.jdbc.JDBCCategoryDataset;
import org.skyve.EXT;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

public class UserMonthlyHits implements DynamicImage<Display> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 920018115413956116L;

	@Override
	public BufferedImage getImage(Display display, int width, int height, User user)
	throws Exception {
		Connection connection = null;
		try {
			StringBuilder sb = new StringBuilder("SELECT ");
			sb.append(" concat(concat(month, '/'), year) as MonthYear");
			sb.append(", sum(numberofHits) as Hits");
			sb.append(" FROM adm_usermonthlyhits ");
			sb.append(" where bizCustomer = '");
			sb.append(user.getCustomer().getName());
			sb.append("\'");
			sb.append(" group by concat(concat(month, '/'), year)");
			
			System.out.println(sb.toString());
			
			connection = EXT.getPooledJDBCConnection();
			JDBCCategoryDataset data = new JDBCCategoryDataset(connection, sb.toString());
			JFreeChart chart = ChartFactory.createBarChart3D("User Hits", "User", "Hits", data, PlotOrientation.VERTICAL, true, false, false);
			
			chart.setBackgroundImageAlpha(0.2F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			
			CategoryPlot plot = (CategoryPlot) chart.getPlot();
		    plot.setRangeGridlinesVisible(false);

		    //BarRenderer renderer = (BarRenderer) plot.getRenderer();
		    BarRenderer renderer = (BarRenderer) chart.getCategoryPlot().getRenderer();
		    
		    plot.setNoDataMessage("No data available");
			plot.setOutlineVisible(false);
			
			Color baseColour = new Color(70, 130, 180);
			Color nextColour = baseColour;
			int redDiff = (baseColour.getRed()/2)/plot.getDataset().getColumnCount();
			int greenDiff = (baseColour.getGreen()/2)/plot.getDataset().getColumnCount();
			int blueDiff = (baseColour.getBlue()/2)/plot.getDataset().getColumnCount();
			
			CategoryItemLabelGenerator generator = new StandardCategoryItemLabelGenerator("{2}", new DecimalFormat("#,##0"));
			for (int seriesIndex = 0; seriesIndex < plot.getDataset().getColumnCount(); seriesIndex++) {
				renderer.setSeriesItemLabelGenerator(seriesIndex, generator);
				renderer.setSeriesItemLabelsVisible(seriesIndex, true);				
				renderer.setSeriesPaint(seriesIndex, nextColour);
				nextColour = new Color(nextColour.getRed()-redDiff,nextColour.getGreen()-greenDiff, nextColour.getBlue()-blueDiff);
				System.out.println("Red " + nextColour.getRed() + " Green " + nextColour.getGreen() + " Blue " + nextColour.getBlue());
			}			

			renderer.setItemLabelAnchorOffset(0);
		    renderer.setBaseItemLabelFont(new Font("Arial Unicode MS", 0, 10));			
			
		    chart.getLegend().setVisible(false);
		    
			return chart.createBufferedImage(width, height);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		finally {
			if (connection != null) {
				try {
					connection.close();
				}
				catch (SQLException e) {
					e.printStackTrace();
				}
				connection = null;
			}
		}

		return null;
	}

	@Override
	public ImageFormat getFormat() {
		return null;
	}

	@Override
	public Float getCompressionQuality() {
		return null;
	}
}
