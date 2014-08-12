package modules.admin.MyContact.actions;

import modules.admin.domain.Contact;
import modules.admin.domain.MyContact;
import modules.admin.domain.Contact.ContactType;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class UpdateMyDetails implements ServerSideAction<MyContact> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4317908281075686229L;

	@Override
	public ServerSideActionResult execute(MyContact bean, WebContext webContext) throws Exception {

		Persistence pers= CORE.getPersistence();
		
		//force person type just in case
		bean.getMyContact().setContactType(ContactType.person);
		Contact myContact = pers.save(bean.getMyContact());
		bean.setMyContact(myContact);
		
		return new ServerSideActionResult(bean); // stay on the same form
	}
}
