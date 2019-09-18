package modules.admin.UserDashboard;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.ModulesUtil;
import modules.admin.User.UserExtension;
import modules.admin.domain.UserDashboard;

public class UserDashboardBizlet extends Bizlet<UserDashboard> {
	private static final long serialVersionUID = -6841455574804123970L;

	@Override
	public UserDashboard newInstance(UserDashboard bean) throws Exception {
		bean.setCurrentUser((UserExtension) ModulesUtil.currentAdminUser());
			
		return bean;
	}
}
