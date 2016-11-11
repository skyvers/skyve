package modules.admin.SystemDashboard.images;

import java.awt.image.BufferedImage;

import modules.admin.ThemeCharter;
import modules.admin.ThemeCharter.ChartAspect;
import modules.admin.domain.SystemDashboard;

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
		ThemeCharter charter = new ThemeCharter();
		charter.setSql(getActivityContextSQL(user, adminUser));
		return charter.getPieChartImage("",new Integer(0), width, height, ChartAspect.FLAT, false);
	}

	@Override
	public ImageFormat getFormat() {
		return null;
	}

	@Override
	public Float getCompressionQuality() {
		return null;
	}

	public static String getActivityContextSQL(User user, modules.admin.domain.User adminUser) {
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
