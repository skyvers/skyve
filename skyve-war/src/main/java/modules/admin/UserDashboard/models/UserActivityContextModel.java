package modules.admin.UserDashboard.models;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.view.model.chart.ChartBuilder;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.chart.OrderBy;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

import modules.admin.ModulesUtil;
import modules.admin.UserDashboard.UserDashboardExtension;
import modules.admin.domain.Audit;
import modules.admin.domain.UserDashboard;

public class UserActivityContextModel extends ChartModel<UserDashboard> {
	@Override
	public ChartData getChartData() {
		// temporarily elevate user to be able to see Audit records in case they don't usually have access
		return CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
			q.getFilter().addGreaterThan(Audit.millisPropertyName, UserDashboardExtension.TWO_WEEKS_AGO);
			q.getFilter().addEquals(Audit.userNamePropertyName, ModulesUtil.currentAdminUser().getUserName());

			ChartBuilder cb = new ChartBuilder();
			cb.with(q);
			cb.category(Audit.auditDocumentNamePropertyName);
			cb.value(Bean.DOCUMENT_ID, AggregateFunction.Count);
			cb.top(6, OrderBy.category, SortDirection.ascending, true);
			cb.orderBy(OrderBy.category, SortDirection.ascending);

			return cb.build("My activity by context - last 14 days", "Context");
		});
	}
}
