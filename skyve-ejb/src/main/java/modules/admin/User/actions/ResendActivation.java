package modules.admin.User.actions;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.User.UserExtension;

/**
 * Resends the registration email with the activation code to the user. Used when a user
 * has lost or did not receive their activation email upon registration.
 */
public class ResendActivation implements ServerSideAction<UserExtension> {

	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension bean, WebContext webContext) throws Exception {
		UserExtension result = bean;
		if (result != null && result.getContact() != null) {
			if (result.getContact().getEmail1() == null) {
				throw new DomainException("This user's contact does not have an email address.");
			}

			// Set activation details
			result.generateActivationDetailsAndSave(CORE.getPersistence());
			result.sendUserRegistrationEmail();

			// Update and save bean for view and action control
			result.setEmailSent(Boolean.TRUE);
			result = CORE.getPersistence().save(result);
		}

		return new ServerSideActionResult<>(result);
	}
}
