package modules.admin.ChangePassword;

import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.security.HIBPPasswordValidator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.ChangePassword;

public class ChangePasswordBizlet extends Bizlet<ChangePassword> {

	@Override
	public void preRerender(String source, ChangePassword bean, WebContext webContext) throws Exception {
		if (ChangePassword.newPasswordPropertyName.equals(source)) {
			String newPassword = bean.getNewPassword();
			if (newPassword != null) {
				if (UtilImpl.CHECK_FOR_BREACHED_PASSWORD) {
					if (HIBPPasswordValidator.isPasswordPwned(newPassword)) {
						webContext.growl(MessageSeverity.warn, Util.i18n("warning.breachedPassword"));
					}
				}
			}
		}

		super.preRerender(source, bean, webContext);
	}
}
