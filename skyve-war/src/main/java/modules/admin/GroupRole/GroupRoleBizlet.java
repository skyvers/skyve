package modules.admin.GroupRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import modules.admin.User.UserService;
import modules.admin.domain.GroupRole;

/**
 * Provides dynamic role domains and conversation-aware resolution for group roles.
 */
public class GroupRoleBizlet extends Bizlet<GroupRole> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient UserService userService;

	/**
	 * Returns available customer roles for the role-name field.
	 *
	 * @param fieldName
	 *        the field requesting variant values
	 * @return customer role values for {@link GroupRole#roleNamePropertyName}, otherwise superclass values
	 * @throws Exception
	 *         if lookup fails
	 */
	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName)
			throws Exception {
		if (fieldName.equals(GroupRole.roleNamePropertyName)) {
			return userService.getCustomerRoleValues(CORE.getUser());
		}

		return super.getVariantDomainValues(fieldName);
	}

	/**
	 * Resolves a candidate group-role row from the active conversation bean.
	 *
	 * @param bizId
	 *        the role identifier to resolve
	 * @param conversationBean
	 *        the current conversation bean, expected to be group/user context
	 * @param webContext
	 *        the active web context
	 * @return the resolved candidate role, or {@code null} when no matching row exists
	 * @throws Exception
	 *         if resolution fails
	 */
	@Override
	public GroupRole resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		if (conversationBean instanceof GroupExtension group) {
			return group.getCandidateRolesElementById(bizId);
		} else if (conversationBean instanceof UserExtension user) {
			GroupExtension group = user.getNewGroup();
			if (group != null) {
				return group.getCandidateRolesElementById(bizId);
			}
		}
		return null;
	}
}
