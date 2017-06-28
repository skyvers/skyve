package modules.admin.util;

import modules.admin.domain.User;

public class UserFactoryExtension extends UserFactory {

	@Override
	public User getInstance() throws Exception {
		User user = super.getInstance();
		user.setConfirmPassword(null);
		user.setNewPassword(null);

		user.getGroups().add(new GroupFactory().getInstance());

		return user;
	}
}
