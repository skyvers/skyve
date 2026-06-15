package modules.admin.UserDashboard.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.UserDashboard;

/**
 * Saves edits to the current user's contact details from the dashboard.
 */
public class UpdateMyDetails implements ServerSideAction<UserDashboard> {
	/**
	 * Forces person contact type, persists the current user, and keeps the form open.
	 *
	 * @param bean The dashboard bean containing the current user reference.
	 * @param webContext The current web context.
	 * @return The same dashboard bean.
	 * @throws Exception If persistence fails.
	 */
	@Override
	public ServerSideActionResult<UserDashboard> execute(UserDashboard bean, WebContext webContext) throws Exception {

		Persistence pers= CORE.getPersistence();
		
		//force person type just in case
		UserExtension me = bean.getCurrentUser();
		me.getContact().setContactType(ContactType.person);
		me = pers.save(me);
		bean.setCurrentUser(me);
		
		return new ServerSideActionResult<>(bean); // stay on the same form
	}
}
