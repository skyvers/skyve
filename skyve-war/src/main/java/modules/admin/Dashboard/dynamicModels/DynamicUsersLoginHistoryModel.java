package modules.admin.Dashboard.dynamicModels;

import java.time.LocalDateTime;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
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
import org.skyve.util.Binder;

import modules.admin.domain.Dashboard;
import modules.admin.domain.Dashboard.GroupingInterval;
import modules.admin.domain.Dashboard.TimeInterval;
import modules.admin.domain.UserLoginRecord;

/**
 * Model for generating a chart of the systems's users login history over the past month.
 * <p>
 * This class builds a chart showing the number of logins per day,
 * using data from {@link UserLoginRecord}. It temporarily elevates permissions to ensure access
 * to the required records.
 */
public class DynamicUsersLoginHistoryModel extends ChartModel<DynamicBean> {

	@Override
	public ChartData getChartData() {

		Persistence pers = CORE.getPersistence();
		DynamicBean bean = getBean();

		// Get the Date that we want the records to start from
		final DateOnly startDateTime;
		TimeInterval timeInterval = (TimeInterval) Binder.get(bean, Dashboard.timeIntervalPropertyName);
		switch (timeInterval) {
			case pastHour:
				startDateTime = new DateOnly(LocalDateTime.now().minusHours(1));
				break;
			case pastDay:
				startDateTime = new DateOnly(LocalDateTime.now().minusDays(1));
				break;
			case pastWeek:
				startDateTime = new DateOnly(LocalDateTime.now().minusWeeks(1));
				break;
			case past2Weeks:
				startDateTime = new DateOnly(LocalDateTime.now().minusWeeks(2));
				break;
			case pastMonth:
				startDateTime = new DateOnly(LocalDateTime.now().minusMonths(1));
				break;
			case pastYear:
				startDateTime = new DateOnly(LocalDateTime.now().minusYears(1));
				break;
			case past5Years:
				startDateTime = new DateOnly(LocalDateTime.now().minusYears(5));
				break;
			default:
				startDateTime = new DateOnly(LocalDateTime.now().minusMonths(1));
				break;
		}

		// temporarily elevate user to be able to see UserLoginRecord records in case they don't usually have access
		return pers.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = pers.newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
			q.getFilter()
					.addGreaterThan(UserLoginRecord.loginDateTimePropertyName, startDateTime);

			ChartBuilder cb = new ChartBuilder();
			cb.with(q);
			// Find temporal bucket to use
			TemporalBucket temporalBucket = new TemporalBucket(TemporalBucketType.dayMonthYear);
			GroupingInterval groupInterval = (GroupingInterval) Binder.get(bean, Dashboard.groupingIntervalPropertyName);
			if (groupInterval!= null) {
				temporalBucket = new TemporalBucket(TemporalBucketType.valueOf(groupInterval.toCode()));
			}
			cb.category(UserLoginRecord.loginDateTimePropertyName, temporalBucket);
			cb.value(Bean.DOCUMENT_ID, AggregateFunction.Count);
			cb.orderBy(OrderBy.category, SortDirection.ascending);

			ChartData chartData = cb.build("Users' logins - last month", "Login Count");
			return chartData;
		});
	}
}
