package modules.admin.Dashboard.models;

import java.time.LocalDateTime;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.view.model.chart.ChartBuilder;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.chart.OrderBy;
import org.skyve.metadata.view.model.chart.TemporalBucket;
import org.skyve.metadata.view.model.chart.TemporalBucket.TemporalBucketType;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;

import modules.admin.ModulesUtil;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.domain.UserLoginRecord;

/**
 * Model for generating a chart of the current user's login history over the past month.
 * <p>
 * This class builds a chart showing the number of logins per day for the current admin user,
 * using data from {@link UserLoginRecord}. It temporarily elevates permissions to ensure access
 * to the required records.
 */
public class MyUserLoginHistoryModel extends ChartModel<DashboardExtension> {

	@Override
	public ChartData getChartData() {

		Persistence pers = CORE.getPersistence();

		// temporarily elevate user to be able to see UserLoginRecord records in case they don't usually have access
		return pers.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = pers.newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
			q.getFilter()
					.addGreaterThan(UserLoginRecord.loginDateTimePropertyName, new DateOnly(LocalDateTime.now().minusMonths(1))); // Records
			q.getFilter().addEquals(UserLoginRecord.userNamePropertyName, ModulesUtil.currentAdminUser().getUserName());

			ChartBuilder cb = new ChartBuilder();
			cb.with(q);
			cb.category(UserLoginRecord.loginDateTimePropertyName, new TemporalBucket(TemporalBucketType.dayMonthYear));
			cb.value(Bean.DOCUMENT_ID, AggregateFunction.Count);
			cb.orderBy(OrderBy.category, SortDirection.ascending);

			ChartData chartData = cb.build("My login history - last month", "Login Count");
			return chartData;
		});
	}
}
