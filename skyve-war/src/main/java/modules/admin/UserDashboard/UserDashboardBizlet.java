package modules.admin.UserDashboard;

import org.skyve.metadata.model.document.Bizlet;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.domain.UserDashboard;

/**
 * Initialises dashboard beans with the current admin user.
 */
public class UserDashboardBizlet extends Bizlet<UserDashboard> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient UserService userService;

	/**
	 * Seeds the dashboard bean with the logged-in admin user.
	 *
	 * @param bean The dashboard bean being initialised.
	 * @return The same bean with current-user populated.
	 * @throws Exception If user lookup fails.
	 */
	@Override
	public UserDashboard newInstance(UserDashboard bean) throws Exception {
		bean.setCurrentUser(userService.currentAdminUser());

		return bean;
	}
}
