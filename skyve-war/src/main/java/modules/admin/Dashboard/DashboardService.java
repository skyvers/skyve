package modules.admin.Dashboard;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import java.lang.String;
import java.util.List;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient DashboardService service;
 */
@Default
public class DashboardService {
	@Inject
	private Persistence persistence;

	public DashboardExtension get(String bizId) {
		final DocumentQuery query = persistence.newDocumentQuery(DashboardExtension.MODULE_NAME, DashboardExtension.DOCUMENT_NAME);
		query.getFilter().addEquals(DashboardExtension.DOCUMENT_ID, bizId);
		return query.beanResult();
	}

	public List<DashboardExtension> getAll() {
		final DocumentQuery query = persistence.newDocumentQuery(DashboardExtension.MODULE_NAME, DashboardExtension.DOCUMENT_NAME);
		return query.beanResults();
	}
}
