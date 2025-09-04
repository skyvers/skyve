package modules.admin.Dashboard.actions;

import java.io.IOException;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.PushMessage;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Dashboard.DashboardExtension;
import modules.admin.Dashboard.DashboardService;
import modules.admin.User.UserExtension;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;

/**
 * ServerSideAction to activate the Dashboard with the configured widgets.
 */
public class ActivateDashboard implements ServerSideAction<DashboardExtension> {
	@Inject
	private transient DashboardService dashboardService;

	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean, WebContext webContext) throws IOException {

		// Activate the Dashboard
		dashboardService.activateDashboard(bean);
		
		// Save Dashboard
		DashboardExtension savedBean = CORE.getPersistence()
				.save(bean);

		// Inform all users with the roles that can access the dashboard
		DocumentQuery qUsers = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		List<UserExtension> users = qUsers.beanResults();

		for (UserExtension user : users) {
			// loop through the roles that can access the dashboard
			for (UserRole role : savedBean.getRoles()) {
				String[] roleParts = role.getRoleName().split("\\.");
				String roleName = roleParts[roleParts.length - 1];

				// Check if user is in role
				if (user.toMetaDataUser().isInRole(savedBean.getModuleName(), roleName)) {
					DefaultRepository repo = (DefaultRepository) CORE.getRepository();
					repo.resetUserPermissions(user.toMetaDataUser());
					EXT.push(new PushMessage().message(MessageSeverity.info, String.format(
							"A new dashboard has been made available for the module %s. Please log out and log back in to view the change.",
							bean.getModuleName())).user(user.toMetaDataUser()));
				}
			}
		}

		// Refresh the page
		dashboardService.redirectToHomeUrl();

		return new ServerSideActionResult<>(savedBean);
	}

}
