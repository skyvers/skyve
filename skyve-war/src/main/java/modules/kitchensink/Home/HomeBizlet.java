package modules.kitchensink.Home;

import java.util.Collection;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.fluent.FluentDocumentPrivilege;
import org.skyve.metadata.module.fluent.FluentModuleRole;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.fluent.FluentView;
import org.skyve.web.WebContext;

import modules.admin.domain.Dashboard;
import modules.kitchensink.domain.Home;
import router.UxUis;

public class HomeBizlet extends SingletonCachedBizlet<Home> {

	@Override
	public Home preExecute(ImplicitActionName actionName, Home bean, Bean parentBean, WebContext webContext) throws Exception {
		if (ImplicitActionName.New.equals(actionName) || ImplicitActionName.Edit.equals(actionName)) {
			if (bean.getDashboard() == null) {
				bean.setDashboard(Dashboard.newInstance());
				bean.getDashboard()
						.loadDashboard();
				Customer customer = CORE.getCustomer();
				Module module = customer.getModule(Dashboard.MODULE_NAME);
				Document dashboardDocument = module.getDocument(customer, Dashboard.DOCUMENT_NAME);
				Module homeModule = customer.getModule(Home.MODULE_NAME);
				Document homeDocument = homeModule.getDocument(customer, Home.DOCUMENT_NAME);
				DefaultRepository repository = (DefaultRepository) CORE.getRepository();

				try {
					LockableDynamicRepository sessionRepository = (LockableDynamicRepository) repository.getSessionRepository();
					if (sessionRepository == null) {
						throw new IllegalStateException(
								"No session repository - this should have been set in ModuleRepositorySkyveObserver.login()");
					}
					sessionRepository.withLock(r -> {
						View dashboardDocumentView = r.getView(UxUis.EXTERNAL.getName(), CORE.getCustomer(), dashboardDocument,
								ViewType.edit.toString());
						Collection<Action> viewActions = r
								.getView(null, CORE.getCustomer(), dashboardDocument, ViewType.edit.toString())
								.getActions();
						ViewImpl homeDocumentView = (ViewImpl) homeDocument.getView(UxUis.EXTERNAL.getName(), customer,
								ViewType.edit.toString());
						viewActions.stream()
								.forEach(a -> {
									if (!homeDocumentView.getActions()
											.contains(a)) {
										homeDocumentView.putAction(a);
									}
								});
						/*FluentModuleRole role = new FluentModuleRole();
						role.name("dev");
						FluentDocumentPrivilege roleDocumentPrivelege = new FluentDocumentPrivilege();
						roleDocumentPrivelege.documentName("Dashboard");
						roleDocumentPrivelege.permission(DocumentPermission._R__C);
						roleDocumentPrivelege.addActionPrivilege("UpdateMyDetails");*/
						r.putView(UxUis.EXTERNAL.getName(), homeDocument, new FluentView().from(dashboardDocumentView)
								.get());
					});
				} catch (Exception e) {
					// revert to vanilla view
					e.printStackTrace();
				}
			}
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

}
