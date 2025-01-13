package modules.admin.DashboardWidget;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import java.lang.String;
import java.util.List;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient DashboardWidgetService service;
 */
@Default
public class DashboardWidgetService {
	@Inject
	private Persistence persistence;

	public DashboardWidgetExtension get(String bizId) {
		final DocumentQuery query = persistence.newDocumentQuery(DashboardWidgetExtension.MODULE_NAME, DashboardWidgetExtension.DOCUMENT_NAME);
		query.getFilter().addEquals(DashboardWidgetExtension.DOCUMENT_ID, bizId);
		return query.beanResult();
	}

	public List<DashboardWidgetExtension> getAll() {
		final DocumentQuery query = persistence.newDocumentQuery(DashboardWidgetExtension.MODULE_NAME, DashboardWidgetExtension.DOCUMENT_NAME);
		return query.beanResults();
	}
}
