package modules.admin.GroupRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.Group.GroupBizlet;
import modules.admin.User.UserBizlet;
import modules.admin.domain.GroupRole;

public class GroupRoleBizlet extends Bizlet<GroupRole> {
	private static final long serialVersionUID = -3329006728591283654L;

	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName)
	throws Exception {
		if (fieldName.equals(GroupRole.roleNamePropertyName)) {
			return UserBizlet.getCustomerRoleValues(CORE.getUser());
		}

		return super.getVariantDomainValues(fieldName);
	}

	@Override
	public GroupRole resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
	
		// Check if the role being resolved is part of the temporary collection for selection
		// in the case of the listMembership selection, the bizId value being resolved
		// will be the roleName
		@SuppressWarnings("unchecked")
		List<DomainValue> roles = (List<DomainValue>) CORE.getStash().get(GroupBizlet.AVAILABLE_ROLES);
		for(DomainValue v: roles) {
			if(v.getCode().equals(bizId)) {
				GroupRole r = GroupRole.newInstance();
				r.setRoleName(v.getCode());
				return r;
			}
		}
		
		return super.resolve(bizId, conversationBean, webContext);
	}
	
	
}
