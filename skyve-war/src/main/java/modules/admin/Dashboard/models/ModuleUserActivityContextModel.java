package modules.admin.Dashboard.models;

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
import org.skyve.persistence.Persistence;

import modules.admin.ModulesUtil;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.domain.Audit;

public class ModuleUserActivityContextModel extends ChartModel<DashboardExtension> {

	@Override
	public ChartData getChartData() {

		Persistence pers = CORE.getPersistence();

		return pers.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
			q.getFilter().addGreaterThan(Audit.millisPropertyName, DashboardExtension.TWO_WEEKS_AGO);
			q.getFilter().addEquals(Audit.userNamePropertyName, ModulesUtil.currentAdminUser().getUserName());
			q.getFilter().addEquals(Audit.auditModuleNamePropertyName, getBean().getModuleMetaData().getName()); // filter for this module activity only

			ChartBuilder cb = new ChartBuilder();
			cb.with(q);
			cb.category(Audit.auditDocumentNamePropertyName);
			cb.value(Bean.DOCUMENT_ID, AggregateFunction.Count);
			cb.top(6, OrderBy.category, SortDirection.ascending, true);
			cb.orderBy(OrderBy.category, SortDirection.ascending);

			ChartData chartData = cb.build("My activity by context - last 14 days", "Context");
			return chartData;
		});
	}
}
