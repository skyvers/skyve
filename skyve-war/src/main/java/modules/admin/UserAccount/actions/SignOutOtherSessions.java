package modules.admin.UserAccount.actions;

import java.util.Objects;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.User.SignOut;
import modules.admin.UserAccount.UserAccountBizlet;
import modules.admin.domain.UserAccount;

/**
 * Signs the current user out of all their other sessions and devices ("Sign out of all other sessions").
 *
 * <p>
 * Removes all the user's remember-me tokens but the one in this browser's cookie, and ends all their sessions but this
 * one. The user is always the signed-in user, never a parameter. Plain "Sign out" in the user menu still ends this device only.
 */
public class SignOutOtherSessions implements ServerSideAction<UserAccount> {
	/**
	 * Signs out the other devices and refreshes the sessions list.
	 *
	 * @param bean The account being viewed.
	 * @param webContext The current web context.
	 * @return The same account, with its sessions refreshed.
	 * @throws Exception If removal fails.
	 */
	@Override
	public ServerSideActionResult<UserAccount> execute(UserAccount bean, WebContext webContext) throws Exception {
		User user = CORE.getUser();
		int signedOut = SignOut.otherDevices(CORE.getPersistence(),
				Objects.requireNonNull(user.getCustomerName()),
				Objects.requireNonNull(user.getName()),
				user.getId(),
				EXT.getHttpServletRequest());
		UserAccountBizlet.populateSessions(bean);

		if (webContext != null) {
			String message = (signedOut == 0) ? Util.i18n("admin.userAccount.actions.signOutOtherSessions.none")
					: Util.i18n("admin.userAccount.actions.signOutOtherSessions.done", String.valueOf(signedOut));
			webContext.growl(MessageSeverity.info, Objects.requireNonNull(message));
		}
		return new ServerSideActionResult<>(bean);
	}
}
