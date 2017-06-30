package modules.admin.util;

import modules.admin.domain.UserDashboard;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/UserDashboardFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
public class UserDashboardFactory extends AbstractDomainFactory<UserDashboard> {

	@Override
	public UserDashboard getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(UserDashboard.MODULE_NAME);
		Document document = module.getDocument(customer, UserDashboard.DOCUMENT_NAME);

		UserDashboard userDashboard = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return userDashboard;
	}
}