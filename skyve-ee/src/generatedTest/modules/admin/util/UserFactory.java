package modules.admin.util;

import modules.admin.domain.User;
import modules.admin.util.ContactFactory;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/UserFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class UserFactory extends AbstractDomainFactory<User > {

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