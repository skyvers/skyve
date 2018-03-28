package modules.admin.UserDashboard;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.UserDashboard;
import modules.admin.util.UserDashboardFactory;

@SkyveFactory(testAction = false)
public class UserDashboardFactoryExtension extends UserDashboardFactory {

	@Override
	public UserDashboard getInstance() throws Exception {
		return super.getInstance();
	}
}
