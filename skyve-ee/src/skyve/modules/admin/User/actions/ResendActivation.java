package modules.admin.User.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;

/**
 * Resends the registration email with the activation code to the user. Used when a user
 * attempts to register an account which already exists but has not been activated.
 */
public class ResendActivation implements ServerSideAction<UserExtension> {

	private static final long serialVersionUID = -1947122266718966646L;

	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension bean, WebContext webContext) throws Exception {

		if (bean != null) {
			if (bean.getContact() == null || bean.getContact().getName() == null) {
//				 this came from a public page, retrieve the user
				UserExtension user = CORE.getPersistence().retrieve(User.MODULE_NAME, User.DOCUMENT_NAME,
						bean.getBizId());
				
//				if (user != null) {
//					bean.setUser(user);
//				}
			}
			
			// Set activation details
			bean.generateActivationDetailsAndSave(CORE.getPersistence());
			
			bean.sendUserRegistrationEmail();
		}

		return new ServerSideActionResult<>(bean);
	}

}
