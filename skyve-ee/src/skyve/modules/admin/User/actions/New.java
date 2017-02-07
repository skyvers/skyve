package modules.admin.User.actions;

import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User;
import modules.admin.domain.User.WizardState;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class New implements ServerSideAction<User> {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 7776867319664519408L;

	@Override
	public ServerSideActionResult<User> execute(User adminUser, WebContext webContext) throws Exception {
	
		// Clear out old matches
		adminUser.getCandidateContacts().clear();
		
		Contact contact = Contact.newInstance();
		String searchContactName = adminUser.getSearchContactName(); 
		if (searchContactName == null) {
			searchContactName = "<New Person>";
		}
		contact.setName(searchContactName);
		contact.setEmail1(adminUser.getSearchEmail());
		contact.setContactType(ContactType.person);
		adminUser.setContact(contact);
		
		adminUser.setWizardState(WizardState.createContact);

		return new ServerSideActionResult<>(adminUser);
	}
}
