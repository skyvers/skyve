package modules.admin.Group;

import java.util.Iterator;
import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.User.UserBizlet;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

public class GroupBizlet extends Bizlet<Group> {

	public static final String AVAILABLE_ROLES = "AVAILABLE_ROLES";
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -1878022453255869159L;

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Group bean) throws Exception {

		if (Group.rolesPropertyName.equals(attributeName)) {
			// Available Roles are defined in metadata, not stored in data
			// so the usual automatic Skyve mechanism for listMembership won't work

			// only display roles not yet selected
			List<DomainValue> declaredRoleNames = UserBizlet.getCustomerRoleValues(CORE.getUser());

			// remove roles already selected
			Iterator<DomainValue> it = declaredRoleNames.iterator();
			while (it.hasNext()) {
				DomainValue v = it.next();
				for (GroupRole role : bean.getRoles()) {
					if (v.getCode().equals(role.getRoleName())) {
						it.remove();
					}
				}
			}
			
			// Stash this collection to be used when resolving these transient GroupRoles
			CORE.getStash().put(AVAILABLE_ROLES, declaredRoleNames);
			
			//now construct a new domain list which uses bizId for the domain value code
			return declaredRoleNames;
		}

		return super.getDynamicDomainValues(attributeName, bean);
	}

}
