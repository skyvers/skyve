package modules.admin.util;

import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;
import modules.admin.util.GroupRoleFactory;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/GroupFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
public class GroupFactory extends AbstractDomainFactory<Group> {

	@Override
	public Group getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Group.MODULE_NAME);
		Document document = module.getDocument(customer, Group.DOCUMENT_NAME);

		Group group = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);
		GroupRole groupRole = new GroupRoleFactory().getInstance();
		group.getRoles().add(groupRole);
		groupRole.setParent(group);

		return group;
	}
}