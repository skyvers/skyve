package modules.admin.util;

import modules.admin.domain.ChangePassword;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class ChangePasswordFactory extends AbstractDomainFactory<ChangePassword> {

	@Override
	public ChangePassword getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(ChangePassword.MODULE_NAME);
		Document document = module.getDocument(customer, ChangePassword.DOCUMENT_NAME);

		ChangePassword changePassword = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return changePassword;
	}
}