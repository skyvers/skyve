package org.skyve.impl.web.spring;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.skyve.impl.security.SkyveRememberMeTokenRepository;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.rememberme.InvalidCookieException;
import org.springframework.security.web.authentication.rememberme.PersistentTokenBasedRememberMeServices;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.Cookie;
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

	/**
	 * Reads the token series from a request's remember-me cookie, to identify this browser's row in the token store.
	 * The cookie value is Base64 of {@code series:token}, as Spring writes it.
	 *
	 * @param request The request, whose cookies are read.
	 * @param cookieName The remember-me cookie's name.
	 * @return The series, or {@code null} if the request has no such cookie or it is not of the form {@code series:token}.
	 */
	public static @Nullable String extractSeries(@Nonnull HttpServletRequest request, @Nonnull String cookieName) {
		Cookie[] cookies = request.getCookies();
		if (cookies == null) {
			return null;
		}
		for (Cookie cookie : cookies) {
			if (cookieName.equals(cookie.getName())) {
				try {
					// as AbstractRememberMeServices.decodeCookie: the value may have lost its padding
					StringBuilder value = new StringBuilder(cookie.getValue());
					while ((value.length() % 4) != 0) {
						value.append('=');
					}
					String[] tokens = new String(Base64.getDecoder().decode(value.toString()), StandardCharsets.UTF_8).split(":", -1);
					// NB Spring URL-encodes each part when it writes the cookie ("+" and "=" of the Base64 series), and the
					// series in the token store is the decoded one
					return ((tokens.length == 2) && (! tokens[0].isEmpty())) ? URLDecoder.decode(tokens[0], StandardCharsets.UTF_8) : null;
				}
				catch (@SuppressWarnings("unused") IllegalArgumentException e) {
					return null; // not Base64, or not URL encoded
				}
			}
		}
		return null;
	}
}
