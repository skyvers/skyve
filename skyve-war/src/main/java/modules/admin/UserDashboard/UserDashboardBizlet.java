package modules.admin.UserDashboard;

import org.skyve.metadata.model.document.Bizlet;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.domain.UserDashboard;

public class UserDashboardBizlet extends Bizlet<UserDashboard> {
	@Inject
	private transient UserService userService;

	@Override
	public UserDashboard newInstance(UserDashboard bean) throws Exception {

		bean.setCurrentUser(userService.currentAdminUser());

		return bean;
	}
}
