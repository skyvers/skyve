package modules.admin.ChangePassword;

import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.security.HIBPPasswordValidator;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.domain.ChangePassword;
import modules.admin.domain.Configuration;

public class ChangePasswordBizlet extends Bizlet<ChangePassword> {

	@Override
	public void preRerender(String source, ChangePassword bean, WebContext webContext) throws Exception {
		if (ChangePassword.newPasswordPropertyName.equals(source)) {
			String newPassword = bean.getNewPassword();
			if (newPassword != null) {
				Configuration c = Configuration.newInstance();
				if (c.isCheckForBreachedPasswordsEnabled()) {
					if (HIBPPasswordValidator.isPasswordPwned(newPassword)) {
						webContext.growl(MessageSeverity.warn,
								"WARNING: The password you have entered has been compromised in a data breach. For security, we recommend selecting a stronger password.");
					}
				}
			}
		}

		super.preRerender(source, bean, webContext);
	}
}
