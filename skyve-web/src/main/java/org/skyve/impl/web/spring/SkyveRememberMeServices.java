package org.skyve.impl.web.spring;

import org.skyve.impl.security.SkyveRememberMeTokenRepository;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.rememberme.InvalidCookieException;
import org.springframework.security.web.authentication.rememberme.PersistentTokenBasedRememberMeServices;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Persistent remember-me services whose sign out ends Remember me in the signing-out browser only.
 *
 * <p>Spring's {@link PersistentTokenBasedRememberMeServices#logout} removes every token series of the user, so
 * signing out on one device also ended Remember me on all the user's other browsers and devices. This removes only the
 * series named in this browser's remember-me cookie, and nothing if there is no usable cookie or no signed-in user.
 * Changing a password still removes all of a user's tokens
 * ({@link SkyveRememberMeTokenRepository#removeUserTokens(org.skyve.persistence.Persistence, String)}), as does
 * Spring when it detects a stolen cookie.
 *
 * <p>If the repository is not a {@link SkyveRememberMeTokenRepository}, which can remove one series, sign out behaves as
 * Spring's does.
 */
public class SkyveRememberMeServices extends PersistentTokenBasedRememberMeServices {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SkyveRememberMeServices.class);

	private final PersistentTokenRepository repository;

	/**
	 * @param key The remember-me key.
	 * @param userDetailsService Loads the user when a remember-me cookie is presented.
	 * @param tokenRepository Persists the token series.
	 */
	public SkyveRememberMeServices(@Nonnull String key,
									@Nonnull UserDetailsService userDetailsService,
									@Nonnull PersistentTokenRepository tokenRepository) {
		super(key, userDetailsService, tokenRepository);
		this.repository = tokenRepository;
	}

	/**
	 * Cancels this browser's remember-me cookie and removes its token series, but no other series of the user.
	 */
	@Override
	public void logout(HttpServletRequest request, HttpServletResponse response, Authentication authentication) {
		if (! (repository instanceof SkyveRememberMeTokenRepository skyveRepository)) {
			super.logout(request, response, authentication);
			return;
		}

		// NB the cookie is read before it is cancelled. Not super.logout() as that removes every series of the user.
		String cookie = extractRememberMeCookie(request);
		cancelCookie(request, response);
		if ((authentication == null) || (cookie == null)) {
			return;
		}
		try {
			String[] tokens = decodeCookie(cookie);
			if (tokens.length == 2) {
				skyveRepository.removeUserToken(authentication.getName(), tokens[0]);
			}
		}
		catch (@SuppressWarnings("unused") InvalidCookieException e) {
			LOGGER.warn("Sign out found an invalid remember-me cookie; no token series removed");
		}
	}
}
