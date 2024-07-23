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
import org.skyve.persistence.Persistence;

import modules.admin.UserDashboard.UserDashboardExtension;
import modules.admin.domain.Audit;
import modules.admin.domain.SystemDashboard;

public class ActivityModel extends ChartModel<SystemDashboard> {

	@Override
	public ChartData getChartData() {
		
		Persistence pers= CORE.getPersistence();
		
		// temporarily elevate user to be able to see Audit records in case they don't usually have access
		pers.setDocumentPermissionScopes(DocumentPermissionScope.global);
		
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addGreaterThan(Audit.millisPropertyName, UserDashboardExtension.TWO_WEEKS_AGO);

		ChartBuilder cb = new ChartBuilder();
		cb.with(q);
		cb.category(Audit.timestampPropertyName, new TemporalBucket(TemporalBucketType.dayMonthYear));
		cb.value(Audit.userNamePropertyName, AggregateFunction.Count);
		cb.top(14, OrderBy.category, SortDirection.descending, false);
		cb.orderBy(OrderBy.category, SortDirection.ascending);
		
		ChartData chartData = cb.build("System activity - last 14 days","Activity");
		
		pers.resetDocumentPermissionScopes();
		
		return chartData;
	}

}
