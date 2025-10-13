package modules.admin.User.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.User.UserExtension;
import modules.admin.User.UserService;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User.WizardState;

public class New implements ServerSideAction<UserExtension> {
	@Inject
	private transient UserService userService;
	
	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension adminUser, WebContext webContext)
	throws Exception {
		// userService null-check for generated tests
		userService = userService == null ? new UserService() : userService; 
		// Clear out old matches
		adminUser.getCandidateContacts().clear();
		
		Contact contact = Contact.newInstance();
		String searchContactName = adminUser.getSearchContactName(); 
		contact.setName(searchContactName);
		contact.setEmail1(adminUser.getSearchEmail());
		contact.setContactType(ContactType.person);
		adminUser.setContact(contact);
		
		adminUser.setWizardState(WizardState.createContact);
		userService.next(adminUser);

		return new ServerSideActionResult<>(adminUser);
	}
}
