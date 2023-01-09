package modules.admin.SelfRegistration.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.User;

/**
 * Resends the registration email with the activation code to the user. Used when a user
 * attempts to register an account which already exists but has not been activated.
 */
public class ResendActivation implements ServerSideAction<SelfRegistrationExtension> {

	@Override
	public ServerSideActionResult<SelfRegistrationExtension> execute(SelfRegistrationExtension bean, WebContext webContext)
			throws Exception {

		if (bean != null) {
			UserExtension user = bean.getUser();
			if (user.getContact() == null || user.getContact().getName() == null) {
				// this came from a public page, retrieve the user
				user = CORE.getPersistence().retrieve(User.MODULE_NAME, User.DOCUMENT_NAME,
						user.getBizId());

				if (user != null) {
					user.setEmailSent(Boolean.TRUE);
					// Set activation details
					user.generateActivationDetailsAndSave(CORE.getPersistence());
					user.sendUserRegistrationEmail();
					user = CORE.getPersistence().save(user);
					// Update and save bean for view and action control
					bean.setUser(user);
				}
			}
		}

		return new ServerSideActionResult<>(bean);
	}

}
