package modules.admin.SystemDashboard.images;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.sql.Connection;
import java.sql.SQLException;

import modules.admin.domain.SystemDashboard;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.labels.PieSectionLabelGenerator;
import org.jfree.chart.labels.StandardPieSectionLabelGenerator;
import org.jfree.chart.plot.PiePlot;
import org.jfree.data.jdbc.JDBCPieDataset;
import org.skyve.EXT;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

public class ActivityContext implements DynamicImage<SystemDashboard> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4758170827473887904L;

	@Override
	public BufferedImage getImage(SystemDashboard display, int width, int height, User user) throws Exception {
		return getActivityContextPieImage(null, width, height, user);
	}

	public static BufferedImage getActivityContextPieImage(modules.admin.domain.User adminUser, int width, int height, User user) throws Exception {
		Connection connection = null;
		try {
			connection = EXT.getPooledJDBCConnection();
			JDBCPieDataset data = new JDBCPieDataset(connection, getActivityContextSQL(user, adminUser));
			JFreeChart chart = ChartFactory.createPieChart("", data, true, false, false);
			chart.setBackgroundImageAlpha(0.2F);
			chart.getPlot().setBackgroundAlpha(0.2F);
			chart.setBackgroundPaint(null);

			PiePlot plot = (PiePlot) chart.getPlot();
			plot.setBackgroundAlpha(0.0F);
			plot.setNoDataMessage("No data available");
			// PieSectionLabelGenerator generator = new
			// StandardPieSectionLabelGenerator("{1}", new
			// DecimalFormat("#,##0"), new DecimalFormat("0.00"));
			PieSectionLabelGenerator generator = new StandardPieSectionLabelGenerator("{0}");
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

	@Override
	public ImageFormat getFormat() {
		return null;
	}

	@Override
	public Float getCompressionQuality() {
		return null;
	}

	public static String getActivityContextSQL(User user, modules.admin.domain.User adminUser) throws MetaDataException {
		StringBuilder sb = new StringBuilder("SELECT ");
		sb.append(" auditDocumentName, count(*) as countOfActivity");
		sb.append(" FROM adm_audit");
		sb.append(" where bizCustomer = '");
		sb.append(user.getCustomer().getName());
		sb.append("\'");
		// filter for user if supplied
		if (adminUser != null) {
			sb.append(" and userName = '").append(adminUser.getUserName()).append("'");
		}

		sb.append(" group by auditDocumentName");

		return sb.toString();
	}
}
