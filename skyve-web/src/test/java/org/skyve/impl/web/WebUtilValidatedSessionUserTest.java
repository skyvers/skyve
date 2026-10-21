package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.Principal;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Tests {@link WebUtil#getValidatedSessionUser(HttpServletRequest)} against the real session registry.
 */
@SuppressWarnings("static-method")
class WebUtilValidatedSessionUserTest {
	private static final String USER_ID = "validatedSessionUserId";
	private static final String SESSION_ID = "validated-session-1";

	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private String originalCustomer;
	private Path cacheDirectory;

	@BeforeEach
	void setUp() throws Exception {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;
		originalCustomer = UtilImpl.CUSTOMER;

		cacheDirectory = Files.createTempDirectory("validated-session-user-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);
		UtilImpl.CUSTOMER = null;

		DefaultCaching.get().shutdown();
		DefaultCaching.get().startup();
		StateUtil.removeSessions(USER_ID);
	}

	@AfterEach
	void tearDown() throws Exception {
		StateUtil.removeSessions(USER_ID);
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
		UtilImpl.SESSION_CACHE = originalSessionCache;
		UtilImpl.CUSTOMER = originalCustomer;

		if (cacheDirectory != null) {
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
					.map(Path::toFile)
					.forEach(File::delete);
			}
		}
	}

	@Test
	void returnsNullWithoutSession() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);

		assertNull(WebUtil.getValidatedSessionUser(request));
		verify(request, never()).getSession(true);
		verify(request, never()).getSession();
	}

	@Test
	void returnsNullWithoutSessionUser() {
		HttpSession session = mock(HttpSession.class);
		HttpServletRequest request = request(session, null);

		assertNull(WebUtil.getValidatedSessionUser(request));
		verify(session, never()).invalidate();
	}

	@Test
	void returnsRegisteredSessionUserWithoutPrincipal() {
		UserImpl user = user();
		HttpSession session = session(user);
		StateUtil.addSession(USER_ID, session);

		assertSame(user, WebUtil.getValidatedSessionUser(request(session, null)));
		verify(session, never()).invalidate();
	}

	@Test
	void returnsRegisteredSessionUserMatchingPrincipal() {
		UserImpl user = user();
		HttpSession session = session(user);
		StateUtil.addSession(USER_ID, session);

		assertSame(user, WebUtil.getValidatedSessionUser(request(session, "demo/tester")));
	}

	@Test
	void invalidatesAndReturnsNullWhenSessionIsNoLongerRegistered() {
		UserImpl user = user();
		HttpSession session = session(user);
		StateUtil.addSession(USER_ID, session);
		StateUtil.removeSessions(USER_ID); // as happens when the user's password changes

		assertNull(WebUtil.getValidatedSessionUser(request(session, "demo/tester")));
		verify(session).invalidate();
	}

	@Test
	void returnsNullWithoutInvalidatingWhenPrincipalIsAnotherUser() {
		UserImpl user = user();
		HttpSession session = session(user);
		StateUtil.addSession(USER_ID, session);

		assertNull(WebUtil.getValidatedSessionUser(request(session, "demo/someoneElse")));
		verify(session, never()).invalidate();
	}

	@Test
	void doesNotSetTheSessionUser() {
		HttpSession session = mock(HttpSession.class);
		HttpServletRequest request = request(session, "demo/tester");

		assertNull(WebUtil.getValidatedSessionUser(request));
		verify(session, never()).setAttribute(any(String.class), any());
	}

	private static UserImpl user() {
		UserImpl result = new UserImpl();
		result.setId(USER_ID);
		result.setName("tester");
		result.setCustomerName("demo");
		return result;
	}

	private static HttpSession session(UserImpl user) {
		HttpSession result = mock(HttpSession.class);
		when(result.getId()).thenReturn(SESSION_ID);
		when(result.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		return result;
	}

	private static HttpServletRequest request(HttpSession session, String principalName) {
		HttpServletRequest result = mock(HttpServletRequest.class);
		when(result.getSession(false)).thenReturn(session);
		if (principalName != null) {
			Principal principal = mock(Principal.class);
			when(principal.getName()).thenReturn(principalName);
			when(result.getUserPrincipal()).thenReturn(principal);
		}
		return result;
	}
}
