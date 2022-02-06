package modules.admin.UserDashboard;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.ModulesUtil;
import modules.admin.domain.UserDashboard;

public class UserDashboardBizlet extends Bizlet<UserDashboard> {

	@Override
	public UserDashboard newInstance(UserDashboard bean) throws Exception {

		bean.setCurrentUser(ModulesUtil.currentAdminUser());

		return bean;
	}
}
