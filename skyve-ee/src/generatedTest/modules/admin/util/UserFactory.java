package modules.admin.util;

import modules.admin.domain.User;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class UserFactory extends AbstractDomainFactory<User> {

	@Override
	public User getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(User.MODULE_NAME);
		Document document = module.getDocument(customer, User.DOCUMENT_NAME);

		User user = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);
		user.setContact(new ContactFactory().getInstance());

		return user;
	}
}