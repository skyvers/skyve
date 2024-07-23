package modules.admin.User.actions;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.MessageSeverity;
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

		if (bean != null && bean.getContact() != null) {
			if (bean.getContact().getEmail1() == null) {
				throw new DomainException("This user's contact does not have an email address.");
			}

			// Set activation details
			bean.generateActivationDetailsAndSave(CORE.getPersistence());
			bean.sendUserRegistrationEmail();

			if (webContext != null) {
				webContext.growl(MessageSeverity.info, "Activation email sent to " + bean.getContact().getEmail1() + ".");
			}
		}

		return new ServerSideActionResult<>(bean);
	}
}
