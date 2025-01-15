package modules.admin.Dashboard.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Contact.ContactType;

public class UpdateMyDetails implements ServerSideAction<DashboardExtension> {
	@Override
	public ServerSideActionResult<DashboardExtension> execute(DashboardExtension bean, WebContext webContext) throws Exception {

		Persistence pers= CORE.getPersistence();
		
		//force person type just in case
		UserExtension me = bean.getUser();
		me.getContact().setContactType(ContactType.person);
		me = pers.save(me);
		bean.setUser(me);
		
		return new ServerSideActionResult<>(bean); // stay on the same form
	}
}
