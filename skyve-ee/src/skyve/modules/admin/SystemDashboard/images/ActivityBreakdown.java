package modules.admin.SystemDashboard.images;

import java.awt.image.BufferedImage;

import modules.admin.ThemeCharter;
import modules.admin.ThemeCharter.ChartAspect;
import modules.admin.domain.SystemDashboard;

import org.jfree.chart.plot.PlotOrientation;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

public class ActivityBreakdown implements DynamicImage<SystemDashboard> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4758170827473887904L;

	@Override
	public BufferedImage getImage(SystemDashboard display, int width, int height, User user) throws Exception {
		ThemeCharter charter = new ThemeCharter();
		charter.setSql(getActivityBreakdownSQL(user));
		return charter.getBarChartImage("", "", null, PlotOrientation.VERTICAL, width, height, ChartAspect.FLAT, false);
	}

	public static String getActivityBreakdownSQL(User user) {

		StringBuilder sb = new StringBuilder("SELECT userName");
		sb.append(", sum(numberofhits) ");
		sb.append(" FROM adm_usermonthlyhits");
		sb.append(" where bizCustomer = '");
		sb.append(user.getCustomer().getName());
		sb.append("\'");
		sb.append(" group by userName");

		return sb.toString();

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
