package org.skyve.impl.cache;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.util.UtilImpl;

import jakarta.servlet.http.HttpSession;

/**
 * Unit tests for user-session tracking helpers in {@link StateUtil}.
 */
@SuppressWarnings({"static-method", "boxing"})
public class SessionTrackingTest {
	private static final String USER_ID = "test-user";

	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private CSRFTokenCacheConfig originalCsrfTokenCache;
	private ConversationCacheConfig originalConversationCache;
	private GeoIPCacheConfig originalGeoIPCache;
	private Path cacheDirectory;

	@Before
	public void before() throws Exception {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;
		originalCsrfTokenCache = UtilImpl.CSRF_TOKEN_CACHE;
		originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		originalGeoIPCache = UtilImpl.GEO_IP_CACHE;

		cacheDirectory = Files.createTempDirectory("skyve-stateutil-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);
		UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(100, 10);
		UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(100, 10);
		UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(100, 10);

		DefaultCaching.get().shutdown();
		DefaultCaching.get().startup();
		StateUtil.removeSessions(USER_ID);
	}

	@After
	public void after() throws Exception {
		StateUtil.removeSessions(USER_ID);
		DefaultCaching.get().shutdown();

		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
		UtilImpl.SESSION_CACHE = originalSessionCache;
		UtilImpl.CSRF_TOKEN_CACHE = originalCsrfTokenCache;
		UtilImpl.CONVERSATION_CACHE = originalConversationCache;
		UtilImpl.GEO_IP_CACHE = originalGeoIPCache;

		if (cacheDirectory != null) {
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a)).map(Path::toFile).forEach(File::delete);
			}
		}
	}

	@Test
	public void hasOtherSessionAndGetSessionCountTrackDistinctSessionIds() {
		HttpSession sessionA = session("A");
		HttpSession sessionB = session("B");

		StateUtil.addSession(USER_ID, sessionA);
		assertThat(StateUtil.getSessionCount(USER_ID), is(1));
		assertThat(StateUtil.hasOtherSession(USER_ID, sessionA), is(false));

		StateUtil.addSession(USER_ID, sessionB);
		assertThat(StateUtil.getSessionCount(USER_ID), is(2));
		assertThat(StateUtil.hasOtherSession(USER_ID, sessionA), is(true));
		assertThat(StateUtil.hasOtherSession(USER_ID, sessionB), is(true));
	}

	@Test
	public void duplicateSessionIdDoesNotCreateOtherSession() {
		HttpSession sessionA1 = session("A");
		HttpSession sessionA2 = session("A");

		StateUtil.addSession(USER_ID, sessionA1);
		StateUtil.addSession(USER_ID, sessionA2);

		assertThat(StateUtil.getSessionCount(USER_ID), is(1));
		assertThat(StateUtil.hasOtherSession(USER_ID, sessionA1), is(false));
	}

	@Test
	public void removeSessionDecrementsCount() {
		HttpSession sessionA = session("A");
		HttpSession sessionB = session("B");

		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.addSession(USER_ID, sessionB);

		StateUtil.removeSession(USER_ID, sessionA);
		assertThat(StateUtil.getSessionCount(USER_ID), is(1));
		assertThat(StateUtil.hasOtherSession(USER_ID, sessionB), is(false));
	}

	@Test
	public void removeSessionRemovesEntryWhenLastSession() {
		HttpSession sessionA = session("A");
		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.removeSession(USER_ID, sessionA);
		assertThat(StateUtil.getSessionCount(USER_ID), is(0));
	}

	@Test
	public void removeSessionOnNonexistentUserDoesNotThrow() {
		HttpSession sessionA = session("A");
		StateUtil.removeSession("no-such-user-" + System.nanoTime(), sessionA); // should not throw
		assertThat(StateUtil.getSessionCount("no-such-user-x"), is(0));
	}

	@Test
	public void checkSessionReturnsTrueForRegisteredSession() {
		HttpSession sessionA = session("check-true-" + System.nanoTime());
		StateUtil.addSession(USER_ID, sessionA);
		try {
			assertThat(StateUtil.checkSession(USER_ID, sessionA), is(true));
		} finally {
			StateUtil.removeSession(USER_ID, sessionA);
		}
	}

	@Test
	public void checkSessionReturnsFalseForUnregisteredSession() {
		HttpSession sessionX = session("unregistered-" + System.nanoTime());
		assertThat(StateUtil.checkSession(USER_ID, sessionX), is(false));
	}

	@Test
	public void checkSessionReturnsFalseAfterRemoval() {
		HttpSession sessionA = session("A-rem-" + System.nanoTime());
		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.removeSession(USER_ID, sessionA);
		assertThat(StateUtil.checkSession(USER_ID, sessionA), is(false));
	}

	@Test
	public void removeSessionsRemovesAllSessionsForUser() {
		StateUtil.addSession(USER_ID, session("S1"));
		StateUtil.addSession(USER_ID, session("S2"));
		StateUtil.removeSessions(USER_ID);
		assertThat(StateUtil.getSessionCount(USER_ID), is(0));
	}

	@Test
	public void getSessionsReturnsRegisteredIds() {
		HttpSession sessionA = session("GA");
		HttpSession sessionB = session("GB");
		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.addSession(USER_ID, sessionB);
		try {
			java.util.Set<String> ids = StateUtil.getSessions(USER_ID);
			assertThat(ids.contains("GA"), is(true));
			assertThat(ids.contains("GB"), is(true));
		} finally {
			StateUtil.removeSessions(USER_ID);
		}
	}

	@Test
	public void hasOtherSessionReturnsFalseWhenNoSessions() {
		HttpSession sessionA = session("no-session-" + System.nanoTime());
		assertThat(StateUtil.hasOtherSession("unknown-user-" + System.nanoTime(), sessionA), is(false));
	}

	@Test
	public void getSessionCountForUnknownUserReturnsZero() {
		assertThat(StateUtil.getSessionCount("unknown-user-" + System.nanoTime()), is(0));
	}

	@Test
	public void replaceTokenAddsTokenForSession() {
		String sessionId = "token-session-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, token);
		assertThat(StateUtil.checkToken(sessionId, token), is(true));
		StateUtil.clearTokens(sessionId);
	}

	@Test
	public void checkTokenReturnsFalseForNullToken() {
		String sessionId = "token-session-null-" + System.nanoTime();
		assertThat(StateUtil.checkToken(sessionId, null), is(false));
	}

	@Test
	public void checkTokenReturnsFalseForMissingSession() {
		String sessionId = "missing-session-" + System.nanoTime();
		assertThat(StateUtil.checkToken(sessionId, Integer.valueOf(42)), is(false));
	}

	@Test
	public void replaceTokenRemovesOldToken() {
		String sessionId = "replace-session-" + System.nanoTime();
		Integer oldToken = StateUtil.createToken();
		Integer newToken = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, oldToken);
		StateUtil.replaceToken(sessionId, oldToken, newToken);
		assertThat(StateUtil.checkToken(sessionId, oldToken), is(false));
		assertThat(StateUtil.checkToken(sessionId, newToken), is(true));
		StateUtil.clearTokens(sessionId);
	}

	@Test
	public void clearTokensRemovesAllTokensForSession() {
		String sessionId = "clear-session-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, token);
		StateUtil.clearTokens(sessionId);
		assertThat(StateUtil.checkToken(sessionId, token), is(false));
	}

	@Test
	public void clearTokensHttpSessionOverloadRemovesAllTokensForSession() {
		HttpSession session = session("clear-http-session-" + System.nanoTime());
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(session.getId(), null, token);
		StateUtil.clearTokens(session);
		assertThat(StateUtil.checkToken(session, token), is(false));
	}

	@Test
	public void replaceTokenHttpSessionOverloadAddsTokenForSession() {
		HttpSession session = session("replace-http-session-" + System.nanoTime());
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(session, null, token);
		assertThat(StateUtil.checkToken(session, token), is(true));
		StateUtil.clearTokens(session);
	}

	@Test
	public void evictExpiredSessionTokensTouchesExistingEntries() {
		String sessionId = "evict-token-session-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, token);
		StateUtil.evictExpiredSessionTokens();
		assertThat(StateUtil.checkToken(sessionId, token), is(true));
		StateUtil.clearTokens(sessionId);
	}

	@Test
	public void evictExpiredConversationsTouchesExistingEntries() {
		String cacheName = UtilImpl.CONVERSATION_CACHE.getName();
		DefaultCaching.get().getEHCache(cacheName, String.class, byte[].class)
						.put("conversation-" + System.nanoTime(), new byte[] { 1, 2, 3 });
		StateUtil.evictExpiredConversations();
		assertThat(DefaultCaching.get().getEHCache(cacheName, String.class, byte[].class) != null, is(true));
	}

	@Test
	public void getGeoIPsReturnsConfiguredCache() {
		assertThat(StateUtil.getGeoIPs(), is(org.hamcrest.CoreMatchers.notNullValue()));
	}

	@Test
	public void logStateStatsRunsWithConfiguredCaches() {
		StateUtil.logStateStats();
		assertThat(StateUtil.getSessionCount(USER_ID), is(0));
	}

	@Test
	public void replaceTokenWithSameOldAndNewDoesNothing() {
		String sessionId = "same-token-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, token, token);
		// token was not stored since old==new
		assertThat(StateUtil.checkToken(sessionId, token), is(false));
	}

	private static HttpSession session(String id) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn(id);
		return session;
	}
}
