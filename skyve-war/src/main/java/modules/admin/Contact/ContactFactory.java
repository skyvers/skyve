package modules.admin.Contact;

import org.skyve.util.test.DataMap;
import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.Contact;

/**
 * Creates Contact document instances used by admin workflows.
 */
@SkyveFactory(value = {
		@DataMap(attributeName = Contact.namePropertyName, fileName = "personName.txt"),
		@DataMap(attributeName = Contact.email1PropertyName, fileName = "email.txt")
})
public class ContactFactory {
	// builder defaults
}
