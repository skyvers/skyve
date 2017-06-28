package modules.admin.util;

import modules.admin.domain.Contact;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class ContactFactory extends AbstractDomainFactory<Contact> {

	@Override
	public Contact getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Contact.MODULE_NAME);
		Document document = module.getDocument(customer, Contact.DOCUMENT_NAME);

		Contact contact = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return contact;
	}
}