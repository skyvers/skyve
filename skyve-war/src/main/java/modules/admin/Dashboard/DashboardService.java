package modules.admin.Dashboard;

import java.io.IOException;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.web.faces.views.MenuView;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import jakarta.el.ELContext;
import jakarta.enterprise.inject.Default;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import modules.admin.domain.Dashboard;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient DashboardService dashboardService;
 */
@Default
public class DashboardService {
	@Inject
	private Persistence persistence;

	/**
	 * Redirects to the document URL for the given DashboardExtension bean
	 */
	@SuppressWarnings("static-method")
	public void redirectToHomeUrl() throws IOException {
		FacesContext fc = FacesContext.getCurrentInstance();
		if (fc != null) {
			fc.getExternalContext().redirect(Util.getHomeUrl());
		}
	}

	/**
	 * Resets the MenuView so that new menu items can be seen
	 */
	@SuppressWarnings("static-method")
	public void resetMenuView() {
		FacesContext fc = FacesContext.getCurrentInstance();
		if (fc != null) {
			ELContext elContext = fc.getELContext();
			MenuView result = (MenuView) elContext.getELResolver().getValue(elContext, null, "menu");
			if (result != null) {
				result.resetState();
			}
		}
	}

	public DashboardExtension get(String bizId) {
		final DocumentQuery query = persistence.newDocumentQuery(Dashboard.MODULE_NAME, Dashboard.DOCUMENT_NAME);
		query.getFilter().addEquals(Bean.DOCUMENT_ID, bizId);
		return query.beanResult();
	}

	public List<DashboardExtension> getAll() {
		final DocumentQuery query = persistence.newDocumentQuery(Dashboard.MODULE_NAME, Dashboard.DOCUMENT_NAME);
		return query.beanResults();
	}
}
