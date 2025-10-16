package modules.admin.Contact;

import jakarta.enterprise.inject.Default;

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
}
