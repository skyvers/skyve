package modules.kitchensink.Home.models;

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
import org.skyve.persistence.Persistence;

import modules.admin.ModulesUtil;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.domain.Audit;
import modules.admin.domain.Dashboard;

public class ModuleUserActivityModel extends ChartModel<DashboardExtension> {

	@Override
	public ChartData getChartData() {
		
		Persistence pers= CORE.getPersistence();
		
		// temporarily elevate user to be able to see Audit records in case they don't usually have access
		return pers.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
			q.getFilter().addGreaterThan(Audit.millisPropertyName, DashboardExtension.TWO_WEEKS_AGO);
			q.getFilter().addEquals(Audit.userNamePropertyName, ModulesUtil.currentAdminUser().getUserName());
			q.getFilter().addEquals(Audit.auditModuleNamePropertyName, Dashboard.MODULE_NAME); // filter for this module activity only

			ChartBuilder cb = new ChartBuilder();
			cb.with(q);
			cb.category(Audit.timestampPropertyName, new TemporalBucket(TemporalBucketType.dayMonthYear));
			cb.value(Audit.userNamePropertyName, AggregateFunction.Count);
			cb.top(14, OrderBy.category, SortDirection.descending, false);
			cb.orderBy(OrderBy.category, SortDirection.ascending);

			ChartData chartData = cb.build("My activity - last 14 days", "Activity");
			return chartData;
		});
	}
}
