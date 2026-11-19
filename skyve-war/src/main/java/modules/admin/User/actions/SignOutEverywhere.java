package modules.admin.User.actions;

import java.util.Objects;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.AccessException;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.User.SignOut;
import modules.admin.User.UserExtension;
import modules.admin.domain.AdminRole;
import modules.admin.domain.UserProxy;

/**
 * Signs a user out of everything, for a security administrator: removes all their remember-me tokens and ends
 * all their sessions.
 *
 * <p>
 * If the administrator targets themself, this browser's session and Remember me stay (as "Sign out of all other
 * sessions" in their account), so the request does not end its own session.
 */
public class SignOutEverywhere implements ServerSideAction<UserExtension> {
	/**
	 * Signs the user out everywhere.
	 *
	 * @param bean The user to sign out.
	 * @param webContext The current web context.
	 * @return The same user.
	 * @throws Exception If the caller is not a security administrator of the user's customer, or removal fails.
	 */
	@Override
	public ServerSideActionResult<UserExtension> execute(UserExtension bean, WebContext webContext) throws Exception {
		User admin = CORE.getUser();
		// The action privilege already limits this to SecurityAdministrators; this is the second check
		if ((!admin.isInRole(AdminRole.SECURITY_ADMINISTRATOR))
				|| (!Objects.equals(admin.getCustomerName(), bean.getBizCustomer()))) {
			throw new AccessException("SignOutEverywhere", admin.getName());
		}

		// The tokens are keyed by the saved user name, not one typed into the form and not yet saved
		Object savedUserName = bean.originalValues().get(UserProxy.userNamePropertyName);
		String userName = (savedUserName instanceof String name) ? name : bean.getUserName();

		String customer = Objects.requireNonNull(bean.getBizCustomer());
		String message;
		if (!bean.isPersisted()) {
			message = Util.i18n("admin.user.actions.signOutEverywhere.notSaved");
		} else if (bean.getBizId().equals(admin.getId())) {
			SignOut.otherDevices(CORE.getPersistence(), customer, userName, bean.getBizId(), EXT.getHttpServletRequest());
			message = Util.i18n("admin.user.actions.signOutEverywhere.self");
		} else {
			SignOut.everywhere(CORE.getPersistence(), customer, userName, bean.getBizId());
			message = Util.i18n("admin.user.actions.signOutEverywhere.done", userName);
		}

		if (webContext != null) {
			webContext.growl(MessageSeverity.info, Objects.requireNonNull(message));
		}
		return new ServerSideActionResult<>(bean);
	}
}
