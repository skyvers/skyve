package modules.admin.SystemDashboard.models;

import org.skyve.CORE;
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

import modules.admin.UserDashboard.UserDashboardExtension;
import modules.admin.domain.Audit;
import modules.admin.domain.SystemDashboard;

/**
 * Produces a day-by-day activity chart of recent audit events.
 */
public class ActivityModel extends ChartModel<SystemDashboard> {
	/**
	 * Builds chart data for activity over the last two weeks.
	 *
	 * @return chart data grouped by day with event counts
	 */
	@Override
	public ChartData getChartData() {
		// temporarily elevate user to be able to see Audit records in case they don't usually have access
		return CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
			q.getFilter().addGreaterThan(Audit.millisPropertyName, UserDashboardExtension.twoWeeksAgo());

			ChartBuilder cb = new ChartBuilder();
			cb.with(q);
			cb.category(Audit.timestampPropertyName, new TemporalBucket(TemporalBucketType.dayMonthYear));
			cb.value(Audit.userNamePropertyName, AggregateFunction.Count);
			cb.top(14, OrderBy.category, SortDirection.descending, false);
			cb.orderBy(OrderBy.category, SortDirection.ascending);
			
			return cb.build("System activity - last 14 days","Activity");
		});
	}
}
