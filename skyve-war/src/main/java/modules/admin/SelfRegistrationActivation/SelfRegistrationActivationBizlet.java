package modules.admin.SelfRegistrationActivation;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Handles activation page entry by resolving the activation code from the
 * request and applying account activation logic.
 */
public class SelfRegistrationActivationBizlet extends Bizlet<SelfRegistrationActivationExtension> {
	/**
	 * Processes activation-code requests for new activation beans.
	 *
	 * @param actionName the implicit action being executed
	 * @param bean the activation bean being prepared
	 * @param parentBean the parent bean for nested contexts, if any
	 * @param webContext the current web context
	 * @return the prepared activation bean
	 * @throws Exception if activation processing fails
	 */
	@Override
	public SelfRegistrationActivationExtension preExecute(ImplicitActionName actionName, SelfRegistrationActivationExtension bean, Bean parentBean, WebContext webContext)
			throws Exception {
		if (ImplicitActionName.New.equals(actionName)) {
			HttpServletRequest request = EXT.getHttpServletRequest();
			String activationCode = request.getParameter("code");

			bean.setUser(bean.activateUser(activationCode));
		}
		return bean;
	}
}
