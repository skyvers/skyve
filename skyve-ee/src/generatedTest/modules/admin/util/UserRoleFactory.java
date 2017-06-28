package modules.admin.util;

import modules.admin.domain.UserRole;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class UserRoleFactory extends AbstractDomainFactory<UserRole> {

	@Override
	public UserRole getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(UserRole.MODULE_NAME);
		Document document = module.getDocument(customer, UserRole.DOCUMENT_NAME);

		UserRole userRole = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return userRole;
	}
}