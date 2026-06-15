package modules.admin.User.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Contact.ContactExtension;
import modules.admin.User.UserExtension;
import modules.admin.User.UserService;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User.WizardState;

/**
 * Starts the new-contact branch of the user wizard.
 */
public class New implements ServerSideAction<UserExtension> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient UserService userService;

	/**
	 * Clears prior candidate matches, seeds a new contact, and advances wizard flow.
	 *
	 * @param adminUser The user wizard bean.
	 * @param webContext The current web context.
	 * @return The same user bean.
	 * @throws Exception If username generation or workflow progression fails.
	 */
	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension adminUser, WebContext webContext)
			throws Exception {
		// Clear out old matches
		adminUser.getCandidateContacts().clear();

		ContactExtension contact = Contact.newInstance();
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
