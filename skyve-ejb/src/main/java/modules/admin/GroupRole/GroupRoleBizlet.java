package modules.admin.GroupRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.Group.GroupExtension;
import modules.admin.User.UserBizlet;
import modules.admin.User.UserExtension;
import modules.admin.domain.GroupRole;

public class GroupRoleBizlet extends Bizlet<GroupRole> {
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
		else if (conversationBean instanceof UserExtension) {
			UserExtension user = (UserExtension) conversationBean;
			GroupExtension group = user.getNewGroup();
			if (group != null) {
				return group.getCandidateRolesElementById(bizId);
			}
		}
		return null;
	}
}
