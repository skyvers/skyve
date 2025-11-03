package modules.admin.Contact;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.enterprise.inject.Default;
import modules.admin.domain.Contact;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient ContactService contactService;
 */
@Default
public class ContactService {
	/**
	 * Returns true if the contact holds no data and can be dispensed with.
	 * 
	 * @param c
	 */
	@SuppressWarnings("static-method")
	public boolean isNothing(ContactExtension c) {
		return c.getName() == null && c.getMobile() == null && c.getEmail1() == null;
	}

	@SuppressWarnings("static-method")
	public Contact getCurrentUserContact() {
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Contact.MODULE_NAME);
		Document document = module.getDocument(customer, Contact.DOCUMENT_NAME);

		Contact contact = persistence.retrieve(document, user.getContactId());

		return contact;
	}
}
