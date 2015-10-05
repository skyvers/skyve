package modules.admin.CurrentUser.images;

import java.awt.image.BufferedImage;

import modules.admin.ThemeCharter;
import modules.admin.Display.images.Activity;
import modules.admin.domain.CurrentUser;

import org.jfree.chart.plot.PlotOrientation;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

public class CurrentUserActivity implements DynamicImage<CurrentUser> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 920018115413956116L;

	/**
	 * Construct a line chart of hits for this user over the last 12 months
	 */
	@Override
	public BufferedImage getImage(CurrentUser bean, int width, int height, User user) throws Exception {

		if (bean != null && bean.getCurrentUser() != null) {
			ThemeCharter charter = new ThemeCharter();
			charter.setSql(Activity.getActivityHistorySQL(bean.getCurrentUser(), user));
			return charter.getAreaChartImage("", "Activity", null, PlotOrientation.VERTICAL, width, height);
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
