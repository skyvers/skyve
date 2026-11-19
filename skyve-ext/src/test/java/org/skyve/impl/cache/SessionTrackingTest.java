package org.skyve.impl.cache;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;

import org.ehcache.Cache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.web.BackgroundTask;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Unit tests for user-session tracking helpers in {@link StateUtil}.
 */
@SuppressWarnings({"static-method", "boxing"})
class SessionTrackingTest {
	private static final String USER_ID = "test-user";

	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private CSRFTokenCacheConfig originalCsrfTokenCache;
	private ConversationCacheConfig originalConversationCache;
	private GeoIPCacheConfig originalGeoIPCache;
	private Path cacheDirectory;

	@BeforeEach
	void before() throws Exception {
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

	@AfterEach
	void after() throws Exception {
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
	void hasOtherSessionAndGetSessionCountTrackDistinctSessionIds() {
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
	void duplicateSessionIdDoesNotCreateOtherSession() {
		HttpSession sessionA1 = session("A");
		HttpSession sessionA2 = session("A");

		StateUtil.addSession(USER_ID, sessionA1);
		StateUtil.addSession(USER_ID, sessionA2);

		assertThat(StateUtil.getSessionCount(USER_ID), is(1));
		assertThat(StateUtil.hasOtherSession(USER_ID, sessionA1), is(false));
	}

	@Test
	void removeSessionDecrementsCount() {
		HttpSession sessionA = session("A");
		HttpSession sessionB = session("B");

		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.addSession(USER_ID, sessionB);

		StateUtil.removeSession(USER_ID, sessionA);
		assertThat(StateUtil.getSessionCount(USER_ID), is(1));
		assertThat(StateUtil.hasOtherSession(USER_ID, sessionB), is(false));
	}

	@Test
	void removeOtherSessionsKeepsOnlyTheGivenSessionAndCountsTheOthers() {
		HttpSession sessionA = session("A");
		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.addSession(USER_ID, session("B"));
		StateUtil.addSession(USER_ID, session("C"));
		StateUtil.addSession("other-user", session("D"));

		assertThat(StateUtil.removeOtherSessions(USER_ID, sessionA), is(2));

		assertThat(StateUtil.getSessions(USER_ID), is(Set.of("A")));
		assertTrue(StateUtil.checkSession(USER_ID, sessionA));
		assertFalse(StateUtil.checkSession(USER_ID, session("B")));
		// another user's sessions are untouched
		assertThat(StateUtil.getSessions("other-user"), is(Set.of("D")));
		StateUtil.removeSessions("other-user");
	}

	@Test
	void removeOtherSessionsWithNothingElseRemovesNothing() {
		HttpSession sessionA = session("A");
		StateUtil.addSession(USER_ID, sessionA);

		assertThat(StateUtil.removeOtherSessions(USER_ID, sessionA), is(0));
		assertThat(StateUtil.getSessions(USER_ID), is(Set.of("A")));
		assertThat(StateUtil.removeOtherSessions("nobody", sessionA), is(0));
	}

	@Test
	void removeOtherSessionsDoesNotRegisterAnUnregisteredSession() {
		StateUtil.addSession(USER_ID, session("B"));

		assertThat(StateUtil.removeOtherSessions(USER_ID, session("A")), is(1));

		assertThat(StateUtil.getSessionCount(USER_ID), is(0));
	}

	@Test
	void removeSessionRemovesEntryWhenLastSession() {
		HttpSession sessionA = session("A");
		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.removeSession(USER_ID, sessionA);
		assertThat(StateUtil.getSessionCount(USER_ID), is(0));
	}

	@Test
	void removeSessionOnNonexistentUserDoesNotThrow() {
		HttpSession sessionA = session("A");
		StateUtil.removeSession("no-such-user-" + System.nanoTime(), sessionA); // should not throw
		assertThat(StateUtil.getSessionCount("no-such-user-x"), is(0));
	}

	@Test
	void checkSessionReturnsTrueForRegisteredSession() {
		HttpSession sessionA = session("check-true-" + System.nanoTime());
		StateUtil.addSession(USER_ID, sessionA);
		try {
			assertThat(StateUtil.checkSession(USER_ID, sessionA), is(true));
		} finally {
			StateUtil.removeSession(USER_ID, sessionA);
		}
	}

	@Test
	void checkSessionReturnsFalseForUnregisteredSession() {
		HttpSession sessionX = session("unregistered-" + System.nanoTime());
		assertThat(StateUtil.checkSession(USER_ID, sessionX), is(false));
	}

	@Test
	void checkSessionReturnsFalseAfterRemoval() {
		HttpSession sessionA = session("A-rem-" + System.nanoTime());
		StateUtil.addSession(USER_ID, sessionA);
		StateUtil.removeSession(USER_ID, sessionA);
		assertThat(StateUtil.checkSession(USER_ID, sessionA), is(false));
	}

	@Test
	void removeSessionsRemovesAllSessionsForUser() {
		StateUtil.addSession(USER_ID, session("S1"));
		StateUtil.addSession(USER_ID, session("S2"));
		StateUtil.removeSessions(USER_ID);
		assertThat(StateUtil.getSessionCount(USER_ID), is(0));
	}

	@Test
	void getSessionsReturnsRegisteredIds() {
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
	void getSessionsReturnsEmptySetForUnknownUser() {
		assertThat(StateUtil.getSessions("unknown-user-" + System.nanoTime()).isEmpty(), is(true));
	}

	@Test
	void getCachedConversationThrowsWhenConversationCacheMisses() {
		String webId = "12345678-1234-1234-1234-123456789012bean-id";

		assertThrows(ConversationEndedException.class, () -> StateUtil.getCachedConversation(webId, null));
	}

	@Test
	void getCachedConversationRestoresConversationForMatchingSession() {
		DynamicBean bean = bean("bean-id");
		TestWebContext context = new TestWebContext("12345678-1234-1234-1234-123456789012", "session-1");
		context.setCurrentBean(bean);
		StateUtil.commitAndCacheConversation(context);

		AbstractWebContext result = StateUtil.getCachedConversation(context.getWebId(), request(session("session-1")));

		assertNotNull(result);
		assertThat(result.getKey(), is("12345678-1234-1234-1234-123456789012"));
		assertThat(result.getCurrentBean().getBizId(), is("bean-id"));
	}

	@Test
	void getCachedConversationRestoresConversationWhenNoRequestIsSupplied() {
		TestWebContext context = new TestWebContext("12345678-1234-1234-1234-123456789012", null);
		context.setCurrentBean(bean("bean-id"));
		StateUtil.commitAndCacheConversation(context);

		AbstractWebContext result = StateUtil.getCachedConversation(context.getWebId(), null);

		assertNotNull(result);
		assertThat(result.getCurrentBean().getBizId(), is("bean-id"));
	}

	@Test
	void getCachedConversationThrowsWhenRequestHasNoSession() {
		TestWebContext context = new TestWebContext("12345678-1234-1234-1234-123456789012", "session-1");
		context.setCurrentBean(bean("bean-id"));
		StateUtil.commitAndCacheConversation(context);

		String webId1 = context.getWebId();
		HttpServletRequest noSessionRequest = request(null);
		assertThrows(SessionEndedException.class, () -> StateUtil.getCachedConversation(webId1, noSessionRequest));
	}

	@Test
	void getCachedConversationThrowsWhenSessionIdDiffers() {
		TestWebContext context = new TestWebContext("12345678-1234-1234-1234-123456789012", "session-1");
		context.setCurrentBean(bean("bean-id"));
		StateUtil.commitAndCacheConversation(context);

		String webId2 = context.getWebId();
		HttpServletRequest differentSessionRequest = request(session("session-2"));
		assertThrows(ConversationEndedException.class, () -> StateUtil.getCachedConversation(webId2, differentSessionRequest));
	}

	@Test
	void getCachedConversationThrowsWhenStoredConversationHasNoSessionId() {
		TestWebContext context = new TestWebContext("12345678-1234-1234-1234-123456789012", null);
		context.setCurrentBean(bean("bean-id"));
		StateUtil.commitAndCacheConversation(context);

		String webId3 = context.getWebId();
		HttpServletRequest differentSessionRequest2 = request(session("session-1"));
		assertThrows(ConversationEndedException.class, () -> StateUtil.getCachedConversation(webId3, differentSessionRequest2));
	}

	@Test
	void hasOtherSessionReturnsFalseWhenNoSessions() {
		HttpSession sessionA = session("no-session-" + System.nanoTime());
		assertThat(StateUtil.hasOtherSession("unknown-user-" + System.nanoTime(), sessionA), is(false));
	}

	@Test
	void getSessionCountForUnknownUserReturnsZero() {
		assertThat(StateUtil.getSessionCount("unknown-user-" + System.nanoTime()), is(0));
	}

	@Test
	void removeTokenRemovesOnlyThatToken() {
		String sessionId = "remove-token-session";
		Integer kept = StateUtil.createToken();
		Integer removed = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, kept);
		StateUtil.replaceToken(sessionId, null, removed);
		StateUtil.removeToken(sessionId, removed);
		StateUtil.removeToken(sessionId, null);
		StateUtil.removeToken("no-such-session", kept);
		assertThat(StateUtil.checkToken(sessionId, removed), is(false));
		assertThat(StateUtil.checkToken(sessionId, kept), is(true));
		StateUtil.clearTokens(sessionId);
	}

	@Test
	void replaceTokenAddsTokenForSession() {
		String sessionId = "token-session-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, token);
		assertThat(StateUtil.checkToken(sessionId, token), is(true));
		StateUtil.clearTokens(sessionId);
	}

	@Test
	void checkTokenReturnsFalseForNullToken() {
		String sessionId = "token-session-null-" + System.nanoTime();
		assertThat(StateUtil.checkToken(sessionId, null), is(false));
	}

	@Test
	void checkTokenReturnsFalseForMissingSession() {
		String sessionId = "missing-session-" + System.nanoTime();
		assertThat(StateUtil.checkToken(sessionId, Integer.valueOf(42)), is(false));
	}

	@Test
	void replaceTokenRemovesOldToken() {
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
	void clearTokensRemovesAllTokensForSession() {
		String sessionId = "clear-session-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, token);
		StateUtil.clearTokens(sessionId);
		assertThat(StateUtil.checkToken(sessionId, token), is(false));
	}

	@Test
	void clearTokensHttpSessionOverloadRemovesAllTokensForSession() {
		HttpSession session = session("clear-http-session-" + System.nanoTime());
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(session.getId(), null, token);
		StateUtil.clearTokens(session);
		assertThat(StateUtil.checkToken(session, token), is(false));
	}

	@Test
	void replaceTokenHttpSessionOverloadAddsTokenForSession() {
		HttpSession session = session("replace-http-session-" + System.nanoTime());
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(session, null, token);
		assertThat(StateUtil.checkToken(session, token), is(true));
		StateUtil.clearTokens(session);
	}

	@Test
	void evictExpiredSessionTokensTouchesExistingEntries() {
		String sessionId = "evict-token-session-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, null, token);
		StateUtil.evictExpiredSessionTokens();
		assertThat(StateUtil.checkToken(sessionId, token), is(true));
		StateUtil.clearTokens(sessionId);
	}

	@Test
	void evictExpiredConversationsTouchesExistingEntries() {
		String cacheName = UtilImpl.CONVERSATION_CACHE.getName();
		Cache<String, byte[]> cache = DefaultCaching.get().getEHCache(cacheName, String.class, byte[].class);
		String conversationKey = "conversation-" + System.nanoTime();
		cache.put(conversationKey, new byte[] { 1, 2, 3 });
		StateUtil.evictExpiredConversations();
		assertNotNull(cache.get(conversationKey));
	}

	@Test
	void getGeoIPsReturnsConfiguredCache() {
		assertThat(StateUtil.getGeoIPs(), is(org.hamcrest.CoreMatchers.notNullValue()));
	}

	@Test
	void logStateStatsRunsWithConfiguredCaches() {
		StateUtil.logStateStats();
		assertThat(StateUtil.getSessionCount(USER_ID), is(0));
	}

	@Test
	void replaceTokenWithAnAlreadyValidNewTokenStillInvalidatesTheOldOne() {
		String sessionId = "already-valid-" + System.nanoTime();
		Integer oldToken = Integer.valueOf(1);
		Integer newToken = Integer.valueOf(2);
		StateUtil.replaceToken(sessionId, null, oldToken);
		StateUtil.replaceToken(sessionId, null, newToken);

		StateUtil.replaceToken(sessionId, oldToken, newToken);
		StateUtil.replaceToken(sessionId, oldToken, newToken); // nothing left to change

		assertThat(StateUtil.checkToken(sessionId, oldToken), is(false));
		assertThat(StateUtil.checkToken(sessionId, newToken), is(true));
		StateUtil.clearTokens(sessionId);
	}

	@Test
	void replaceTokenWithSameOldAndNewDoesNothing() {
		String sessionId = "same-token-" + System.nanoTime();
		Integer token = StateUtil.createToken();
		StateUtil.replaceToken(sessionId, token, token);
		// token was not stored since old==new
		assertThat(StateUtil.checkToken(sessionId, token), is(false));
	}

	@Test
	void getSessionsReturnsAReadOnlySnapshot() {
		HttpSession first = session("first");
		StateUtil.addSession("snapshot-user", first);
		Set<String> snapshot = StateUtil.getSessions("snapshot-user");

		StateUtil.addSession("snapshot-user", session("second"));

		assertEquals(Set.of("first"), snapshot);
		assertThrows(UnsupportedOperationException.class, () -> snapshot.add("forged"));
		assertEquals(2, StateUtil.getSessionCount("snapshot-user"));
	}

	@Test
	void removingTheLastSessionRemovesTheUsersEntryAndSecondCallsChangeNothing() {
		HttpSession session = session("only");
		StateUtil.addSession("last-user", session);
		StateUtil.addSession("last-user", session);
		assertEquals(1, StateUtil.getSessionCount("last-user"));

		StateUtil.removeSession("last-user", session("other"));
		assertEquals(1, StateUtil.getSessionCount("last-user"));
		StateUtil.removeSession("last-user", session);
		StateUtil.removeSession("last-user", session);

		assertFalse(sessionsCache().containsKey("last-user"));
	}

	@Test
	void clearingTokensKeepsAnEmptyEntryOnlyForAKnownSession() {
		StateUtil.replaceToken("known", null, Integer.valueOf(1));
		StateUtil.removeToken("known", Integer.valueOf(1));
		assertTrue(tokensCache().containsKey("known"));

		StateUtil.replaceToken("cleared", null, Integer.valueOf(2));
		StateUtil.clearTokens("cleared");
		StateUtil.clearTokens("never-seen");

		assertFalse(StateUtil.checkToken("cleared", Integer.valueOf(2)));
		assertTrue(tokensCache().containsKey("cleared"));
		assertFalse(tokensCache().containsKey("never-seen"));
	}

	@SuppressWarnings("rawtypes")
	private static Cache<String, TreeSet> sessionsCache() {
		return DefaultCaching.get().getEHCache(UtilImpl.SESSION_CACHE.getName(), String.class, TreeSet.class);
	}

	@SuppressWarnings("rawtypes")
	private static Cache<String, TreeSet> tokensCache() {
		return DefaultCaching.get().getEHCache(UtilImpl.CSRF_TOKEN_CACHE.getName(), String.class, TreeSet.class);
	}

	private static HttpSession session(String id) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn(id);
		return session;
	}

	private static HttpServletRequest request(HttpSession session) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		return request;
	}

	private static DynamicBean bean(String bizId) {
		Map<String, Object> properties = new java.util.HashMap<>();
		properties.put(Bean.DOCUMENT_ID, bizId);
		return new DynamicBean("admin", "User", properties);
	}

	private static final class TestWebContext extends AbstractWebContext {
		private static final long serialVersionUID = 1L;

		private TestWebContext(String key, String sessionId) {
			super(key);
			this.sessionId = sessionId;
		}

		@Override
		public List<Map<String, String>> getGrowls() {
			return Collections.emptyList();
		}

		@Override
		public List<Map<String, String>> getMessages() {
			return Collections.emptyList();
		}

		@Override
		public void message(MessageSeverity severity, String message) {
			// no-op for cache serialization tests
		}

		@Override
		public void growl(MessageSeverity severity, String message) {
			// no-op for cache serialization tests
		}

		@Override
		public void cacheConversationAndCycleTransaction() {
			StateUtil.commitAndCacheConversation(this);
		}

		@Override
		public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) {
			// no-op for cache serialization tests
		}

		@Override
		public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) {
			// no-op for cache serialization tests
		}
	}
}
