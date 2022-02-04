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
			if (bean.getUser().getContact() == null || bean.getUser().getContact().getName() == null) {
				// this came from a public page, retrieve the user
				UserExtension user = CORE.getPersistence().retrieve(User.MODULE_NAME, User.DOCUMENT_NAME,
						bean.getUser().getBizId());

				if (user != null) {
					bean.setUser(user);
				}
			}

			// Set activation details
			bean.getUser().generateActivationDetailsAndSave(CORE.getPersistence());

			bean.getUser().sendUserRegistrationEmail();
		}

		return new ServerSideActionResult<>(bean);
	}

}
