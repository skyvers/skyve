package modules.admin.SystemDashboard.images;

import java.awt.image.BufferedImage;
import java.text.DateFormatSymbols;
import java.util.Calendar;
import java.util.Date;

import modules.admin.ThemeCharter;
import modules.admin.domain.SystemDashboard;

import org.jfree.chart.plot.PlotOrientation;
import org.skyve.impl.util.TimeUtil;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

public class Activity implements DynamicImage<SystemDashboard> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 920018115413956116L;

	@Override
	public BufferedImage getImage(SystemDashboard display, int width, int height, User user) throws Exception {
		ThemeCharter charter = new ThemeCharter();
		charter.setSql(getActivityHistorySQL(null, user));
		return charter.getAreaChartImage("", "Activity", null, PlotOrientation.VERTICAL, width, height, false);
	}

	public static String getActivityHistorySQL(modules.admin.domain.User adminUser, User user) throws Exception {
		// note SQL concat function is implementation specific
		StringBuilder sqlB = new StringBuilder(512);

		sqlB.append("select monthName , hits from (");
		sqlB.append(" select year, case ");

		// construct month names from int
		DateFormatSymbols dfs = new DateFormatSymbols();
		String[] months = dfs.getMonths();
		for (int m = 0; m < 11; m++) {
			sqlB.append(" when month = ").append(m + 1).append(" then '").append(months[m].substring(0, 3)).append("'");
		}
		sqlB.append(" else 'Dec' end as monthName, month");
		sqlB.append(", hits from (");

		sqlB.append(" select year, month, sum(numberOfHits) as hits");
		sqlB.append(" from (");
		sqlB.append("	select year, month, numberOfHits");
		sqlB.append("	from adm_userMonthlyHits");

		sqlB.append(" where bizCustomer = '");
		sqlB.append(user.getCustomer().getName());
		sqlB.append("\'");

		// filter for user if supplied
		if (adminUser != null) {
			sqlB.append(" and userName = '").append(adminUser.getUserName()).append("'");
		}

		// union in all possible year/month combinations
		// for the last 12 months
		Calendar c = Calendar.getInstance();
		Date now = new Date();
		for (int i = 0; i < 12; i++) {
			TimeUtil.addMonths(now, -1);
			c.setTime(now);
			sqlB.append("	union select ");
			sqlB.append(c.get(Calendar.YEAR)).append(',');
			sqlB.append(c.get(Calendar.MONTH) + 1).append(',');
			sqlB.append('0');
		}
		sqlB.append(" ) rawHits");

		// filter for last 12 months
		int lastYear = c.get(Calendar.YEAR) * 100 + (c.get(Calendar.MONTH) + 1);
		sqlB.append(" where (year*100+month) > ").append(lastYear);

		// grouping and order
		sqlB.append(" group by year, month");
		sqlB.append(" ) sumHits");
		sqlB.append(" ) sumHitsMonth");
		sqlB.append(" order by year , month ");

		return sqlB.toString();
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
