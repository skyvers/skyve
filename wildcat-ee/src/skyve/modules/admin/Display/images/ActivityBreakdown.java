package modules.admin.Display.images;

import java.awt.image.BufferedImage;

import modules.admin.ThemeCharts;
import modules.admin.ThemeCharts.ChartAspect;
import modules.admin.domain.Display;

import org.jfree.chart.plot.PlotOrientation;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

public class ActivityBreakdown implements DynamicImage<Display> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4758170827473887904L;

	@Override
	public BufferedImage getImage(Display display, int width, int height, User user) throws Exception {
		return ThemeCharts.getBarChartImage(getActivityBreakdownSQL(user), "","",null, PlotOrientation.VERTICAL, width, height, ChartAspect.FLAT);
	}

	public static String getActivityBreakdownSQL(User user) throws MetaDataException {

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
