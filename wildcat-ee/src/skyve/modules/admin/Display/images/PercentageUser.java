package modules.admin.Display.images;

import java.awt.image.BufferedImage;
import java.sql.Connection;
import java.sql.SQLException;

import modules.admin.domain.Display;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.data.jdbc.JDBCPieDataset;

import org.skyve.CORE;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

public class PercentageUser implements DynamicImage<Display> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4758170827473887904L;

	@Override
	public BufferedImage getImage(Display display, int width, int height, User user)
	throws Exception {
		Connection connection = null;
		try {
			StringBuilder sb = new StringBuilder("SELECT userName as User");
			sb.append(", sum(numberofhits) ");
			sb.append(" FROM adm_usermonthlyhits");
			sb.append(" where bizCustomer = '");
			sb.append(user.getCustomer().getName());
			sb.append("\'");
			sb.append(" group by User");

			connection = CORE.getPooledConnection();
			JDBCPieDataset data = new JDBCPieDataset(connection, sb.toString());
			JFreeChart chart = ChartFactory.createPieChart3D("User", data, true, false, false);
			chart.setBackgroundImageAlpha(0F);
			chart.getPlot().setBackgroundAlpha(0F);
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
