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
		boolean result = true;

		result = result && (c.getName() == null);
		result = result && (c.getMobile() == null);
		result = result && (c.getEmail1() == null);

		return result;
	}
}
