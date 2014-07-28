package modules.admin.User.actions;

import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.User.WizardState;
import modules.admin.domain.User;
import modules.admin.domain.UserCandidateContact;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class New implements ServerSideAction<User> {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 7776867319664519408L;

	@Override
	public ServerSideActionResult execute(User adminUser, WebContext webContext) throws Exception {
		Persistence persistence = CORE.getPersistence();
		org.skyve.metadata.user.User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(UserCandidateContact.MODULE_NAME);
		Document document = module.getDocument(customer, Contact.DOCUMENT_NAME);

		// Clear out old matches
		adminUser.getCandidateContacts().clear();
		
		Contact contact = document.newInstance(user);
		String searchContactName = adminUser.getSearchContactName(); 
		if (searchContactName == null) {
			searchContactName = "<New Person>";
		}
		contact.setName(searchContactName);
		contact.setEmail1(adminUser.getSearchEmail());
		contact.setContactType(ContactType.person);
		adminUser.setContact(contact);
		
		adminUser.setWizardState(WizardState.createContact);

		return new ServerSideActionResult(adminUser);
	}
}
