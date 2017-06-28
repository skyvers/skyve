package modules.admin.util;

import modules.admin.domain.GroupRole;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

public class GroupRoleFactory extends AbstractDomainFactory<GroupRole> {

	@Override
	public GroupRole getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(GroupRole.MODULE_NAME);
		Document document = module.getDocument(customer, GroupRole.DOCUMENT_NAME);

		GroupRole groupRole = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return groupRole;
	}
}