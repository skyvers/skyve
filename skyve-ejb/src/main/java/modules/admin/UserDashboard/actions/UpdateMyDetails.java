package modules.admin.UserDashboard.actions;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.UserDashboard;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class UpdateMyDetails implements ServerSideAction<UserDashboard> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4317908281075686229L;

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
