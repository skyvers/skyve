package modules.admin.util;

import modules.admin.domain.UserCandidateContact;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class UserCandidateContactFactory extends AbstractDomainFactory<UserCandidateContact> {

	@Override
	public UserCandidateContact getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(UserCandidateContact.MODULE_NAME);
		Document document = module.getDocument(customer, UserCandidateContact.DOCUMENT_NAME);

		UserCandidateContact userCandidateContact = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return userCandidateContact;
	}
}