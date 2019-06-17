package modules.admin.GroupRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.Group.GroupExtension;
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
		if (conversationBean instanceof GroupExtension) {
			return ((GroupExtension) conversationBean).getCandidateRolesElementById(bizId);
		}
		
		return super.resolve(bizId, conversationBean, webContext);
	}
}
