package modules.admin.UserDashboard;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.ModulesUtil;
import modules.admin.domain.UserDashboard;

public class UserDashboardBizlet extends Bizlet<UserDashboard> {

	private static final long serialVersionUID = -4267161840200612498L;

	@Override
	public UserDashboard newInstance(UserDashboard bean) throws Exception {

		bean.setCurrentUser(ModulesUtil.currentAdminUser());

		return bean;
	}
}
