package modules.admin.SelfRegistration.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Contact.ContactExtension;
import modules.admin.SelfRegistration.SelfRegistrationExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.AdminDocument;

/**
 * Resends the registration email with the activation code to the user. Used when a user
 * attempts to register an account which already exists but has not been activated.
 */
public class ResendActivation implements ServerSideAction<SelfRegistrationExtension> {
	/**
	 * Reissues activation details and sends a fresh registration email.
	 *
	 * @param bean the registration bean containing the user to reactivate
	 * @param webContext the current web context
	 * @return the action result wrapping the supplied bean
	 * @throws Exception if user retrieval, activation update, or email delivery
	 *         fails
	 */
	@Override
	public ServerSideActionResult<SelfRegistrationExtension> execute(SelfRegistrationExtension bean, WebContext webContext)
			throws Exception {
		if (bean != null) {
			ContactExtension contact = bean.getUser().getContact();
			if (contact == null || contact.getName() == null) {
				// this came from a public page, retrieve the user
				UserExtension user = CORE.getPersistence().retrieve(AdminDocument.USER, bean.getUser().getBizId());

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
