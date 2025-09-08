package modules.admin.Dashboard;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil;
import modules.admin.domain.Dashboard;

public class DynamicDashboardBizlet extends Bizlet<DynamicBean> {

	@Override
	public DynamicBean preExecute(ImplicitActionName actionName, DynamicBean bean, Bean parentBean,
			WebContext webContext)
			throws Exception {
		if (ImplicitActionName.New.equals(actionName) || ImplicitActionName.Edit.equals(actionName)) {
			// Set the user to current admin user
			Binder.set(bean, Dashboard.userPropertyName, ModulesUtil.currentAdminUser());
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}
}
