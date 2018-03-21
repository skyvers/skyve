package modules.admin.User;

import org.skyve.util.test.SkyveFactory;

import modules.admin.User.actions.Check;
import modules.admin.User.actions.Next;
import modules.admin.domain.User;
import modules.admin.util.GroupFactory;
import modules.admin.util.UserFactory;

@SkyveFactory(excludedActions = { Check.class, Next.class })
public class UserFactoryExtension extends UserFactory {

	@Override
	public User getInstance() throws Exception {
		User user = super.getInstance();
		user.setConfirmPassword(null);
		user.setGeneratedPassword(null);
		user.setNewPassword(null);

		user.getGroups().add(new GroupFactory().getInstance());

		return user;
	}
}
