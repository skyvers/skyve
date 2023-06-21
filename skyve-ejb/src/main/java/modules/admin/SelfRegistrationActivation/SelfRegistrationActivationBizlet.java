package modules.admin.SelfRegistrationActivation;

import javax.servlet.http.HttpServletRequest;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

public class SelfRegistrationActivationBizlet extends Bizlet<SelfRegistrationActivationExtension> {
	@Override
	public SelfRegistrationActivationExtension preExecute(ImplicitActionName actionName, SelfRegistrationActivationExtension bean, Bean parentBean, WebContext webContext)
			throws Exception {
		if (ImplicitActionName.New.equals(actionName)) {
			HttpServletRequest request = (HttpServletRequest) webContext.getHttpServletRequest();
			String activationCode = request.getParameter("code");

			bean.setUser(bean.activateUser(activationCode));
		}
		return bean;
	}
}
