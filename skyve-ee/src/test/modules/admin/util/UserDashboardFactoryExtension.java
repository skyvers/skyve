package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.UserDashboard;

@SkyveFactory(testAction = false)
public class UserDashboardFactoryExtension extends UserDashboardFactory {

	@Override
	public UserDashboard getInstance() throws Exception {
		return super.getInstance();
	}
}
