package modules.admin.DashboardDesigner;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import java.lang.String;
import java.util.List;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient DashboardDesignerService service;
 */
@Default
public class DashboardDesignerService {
	@Inject
	private Persistence persistence;

	public DashboardDesignerExtension get(String bizId) {
		final DocumentQuery query = persistence.newDocumentQuery(DashboardDesignerExtension.MODULE_NAME, DashboardDesignerExtension.DOCUMENT_NAME);
		query.getFilter().addEquals(DashboardDesignerExtension.DOCUMENT_ID, bizId);
		return query.beanResult();
	}

	public List<DashboardDesignerExtension> getAll() {
		final DocumentQuery query = persistence.newDocumentQuery(DashboardDesignerExtension.MODULE_NAME, DashboardDesignerExtension.DOCUMENT_NAME);
		return query.beanResults();
	}
}
