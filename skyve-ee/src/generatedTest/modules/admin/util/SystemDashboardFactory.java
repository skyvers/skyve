package modules.admin.util;

import modules.admin.domain.SystemDashboard;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/SystemDashboardFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
public class SystemDashboardFactory extends AbstractDomainFactory<SystemDashboard> {

	@Override
	public SystemDashboard getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(SystemDashboard.MODULE_NAME);
		Document document = module.getDocument(customer, SystemDashboard.DOCUMENT_NAME);

		SystemDashboard systemDashboard = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return systemDashboard;
	}
}