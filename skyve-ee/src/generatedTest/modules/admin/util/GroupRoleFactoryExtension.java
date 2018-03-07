package modules.admin.util;

import modules.admin.domain.GroupRole;

public class GroupRoleFactoryExtension extends GroupRoleFactory {
	@Override
	public GroupRole getInstance() throws Exception {
		GroupRole result = super.getInstance();
		result.setRoleName("admin.BasicUser");
		return result;
	}
}
