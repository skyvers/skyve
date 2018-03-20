package modules.admin.util;

import modules.admin.domain.ChangePassword;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/ChangePasswordFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class ChangePasswordFactory extends AbstractDomainFactory<ChangePassword > {

	@Override
	public ChangePassword getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(ChangePassword.MODULE_NAME);
		Document document = module.getDocument(customer, ChangePassword.DOCUMENT_NAME);

		ChangePassword changePassword = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return changePassword;
	}
}