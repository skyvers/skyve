package modules.admin.UserList;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.admin.domain.Group;
import modules.admin.domain.UserList;

public class UserListBizlet extends Bizlet<UserList> {

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		Persistence persistence = CORE.getPersistence();
		Customer customer = persistence.getUser().getCustomer();

		if (UserList.defaultModuleNamePropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>(customer.getModules().size());
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getLocalisedTitle()));
			}
			return result;
		}
		else if (UserList.userInvitationGroupsPropertyName.equals(attributeName)) {
			DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
			query.addBoundOrdering(Group.namePropertyName, SortDirection.ascending);
			List<Group> groups = query.beanResults();
			List<DomainValue> result = new ArrayList<>(groups.size());
			for (Group group : groups) {
				result.add(new DomainValue(group.getBizId(), group.getBizKey()));
			}
			return result;
		}
		return super.getVariantDomainValues(attributeName);
	}

}
