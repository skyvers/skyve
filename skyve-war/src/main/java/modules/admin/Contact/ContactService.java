package modules.admin.Contact;

import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import modules.admin.domain.Contact;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient ContactService contactService;
 */
@Default
public class ContactService {
	@Inject
	Persistence persistence;

	/**
	 * Returns true if the contact holds no data and can be dispensed with.
	 * 
	 * @param c
	 */
	@SuppressWarnings("static-method")
	public boolean isNothing(ContactExtension c) {
		return c.getName() == null && c.getMobile() == null && c.getEmail1() == null;
	}

	/**
	 * Retrieves the contact associated with the current session user.
	 * 
	 * @return The Contact associated with the current user
	 */
	public Contact getCurrentUserContact() {
		User user = persistence.getUser();
		Contact contact = persistence.retrieve(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, user.getContactId());

		return contact;
	}
}
