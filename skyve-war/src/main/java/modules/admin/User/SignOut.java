package modules.admin.User;

import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.security.SkyveRememberMeTokenRepository;
import org.skyve.impl.web.spring.SkyveRememberMeServices;
import org.skyve.impl.web.spring.SpringSecurityConfig;
import org.skyve.persistence.Persistence;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Ends a user's remember-me tokens and sessions, for every device or for every device but this one.
 *
 * <p>Remember-me tokens are keyed by {@code customer/userName} in {@code ADM_UserToken} and sessions by the user's
 * bizId in the session cache ({@link StateUtil}); a session ends when it is unregistered there.
 */
public final class SignOut {
	private SignOut() {
		// Disallow instantiation.
	}

	/**
	 * Removes all of a user's remember-me tokens (in the persistence's transaction) and ends all their sessions.
	 *
	 * @param persistence The persistence to delete with.
	 * @param customerName The user's customer.
	 * @param userName The user name the tokens are keyed by.
	 * @param userId The user's bizId.
	 */
	public static void everywhere(@Nonnull Persistence persistence,
									@Nonnull String customerName,
									@Nonnull String userName,
									@Nonnull String userId) {
		new SkyveRememberMeTokenRepository().removeUserTokens(persistence, customerName + '/' + userName);
		StateUtil.removeSessions(userId);
	}

	/**
	 * Removes a user's remember-me tokens and ends their sessions, except this browser's: the series in the request's
	 * remember-me cookie (if any) and the request's session (if it is registered).
	 *
	 * @param persistence The persistence to delete with.
	 * @param customerName The user's customer.
	 * @param userName The user name the tokens are keyed by.
	 * @param userId The user's bizId.
	 * @param request The current request, from the browser to keep signed in.
	 * @return The number of other devices signed out: the larger of the sessions ended and the token series removed
	 * 			(one device usually has one of each, so adding them would count it twice).
	 */
	public static int otherDevices(@Nonnull Persistence persistence,
									@Nonnull String customerName,
									@Nonnull String userName,
									@Nonnull String userId,
									@Nonnull HttpServletRequest request) {
		String series = SkyveRememberMeServices.extractSeries(request, SpringSecurityConfig.REMEMBER_ME_PARAMETER);
		int tokens = new SkyveRememberMeTokenRepository().removeOtherUserTokens(persistence, customerName + '/' + userName, series);

		int sessions = 0;
		HttpSession session = request.getSession(false);
		if (session == null) {
			sessions = StateUtil.getSessionCount(userId);
			StateUtil.removeSessions(userId);
		}
		else {
			sessions = StateUtil.removeOtherSessions(userId, session);
		}
		return Math.max(tokens, sessions);
	}
}
