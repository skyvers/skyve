package modules.admin.Dashboard.actions;

import org.skyve.CORE;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.User.UserExtension;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.Dashboard;

public class DynamicUpdateMyDetails implements ServerSideAction<DynamicBean> {
	@Override
	public ServerSideActionResult<DynamicBean> execute(DynamicBean bean, WebContext webContext) throws Exception {

		Persistence pers= CORE.getPersistence();
		
		//force person type just in case
		UserExtension me = (UserExtension) Binder.get(bean, Dashboard.userPropertyName);
		me.getContact().setContactType(ContactType.person);
		me = pers.save(me);
		Binder.set(bean, Dashboard.userPropertyName, me);
		
		return new ServerSideActionResult<>(bean); // stay on the same form
	}
}
