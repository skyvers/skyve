package modules.admin.Configuration;

import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.SingletonCachedBizlet;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.domain.Configuration;
import modules.admin.domain.Contact;
import modules.admin.domain.User;

public class ConfigurationBizlet extends SingletonCachedBizlet<Configuration> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1282437688681930236L;

	@Override
	public Configuration newInstance(Configuration bean) throws Exception {
		Configuration result = super.newInstance(bean);

		if (result.getPasswordComplexityModel() == null) {
			result.setPasswordComplexityModel(ComplexityModel.DEFAULT_COMPLEXITY_MODEL);
		}
		if (result.getFromEmail() == null) {
			result.setFromEmail(UtilImpl.SMTP_SENDER);
		}
		if (result.getPasswordResetEmailSubject() == null) {
			result.setPasswordResetEmailSubject("Password Reset");
		}
		if (result.getPasswordResetEmailBody() == null) {
			String body = String.format("<html><head/><body>Hi {%s},<p/>" +
					"Please click below to reset your password.<p/>" +
					"<a href=\"{url}/pages/resetPassword.jsp?t={%s}\">" +
					"Reset Password</a></body></html>",
					Binder.createCompoundBinding(User.contactPropertyName, Contact.namePropertyName),
					User.passwordResetTokenPropertyName);
			result.setPasswordResetEmailBody(body);
		}

		return result;
	}

	@Override
	public Configuration preExecute(ImplicitActionName actionName, Configuration bean, Bean parentBean, WebContext webContext) throws Exception {

		if (ImplicitActionName.Save.equals(actionName) || ImplicitActionName.OK.equals(actionName)) {
			if (bean.getUserSelfRegistrationGroup() == null) {
				bean.setAllowUserSelfRegistration(Boolean.FALSE);
			}
			
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}

}
