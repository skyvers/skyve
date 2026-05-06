package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
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
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

/**
 * Unit tests for concurrent-session warning decision logic in {@link WebUtil}.
 */
class WebUtilConcurrentSessionTest {
	private static final String USER_ID = "test-user";

	private ProvidedRepository originalRepository;
	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private Path cacheDirectory;

	@BeforeEach
	void beforeEach() throws Exception {
		originalRepository = ProvidedRepositoryFactory.get();
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;

		cacheDirectory = Files.createTempDirectory("skyve-webutil-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);

		DefaultCaching.get().shutdown();
		DefaultCaching.get().startup();
		StateUtil.removeSessions(USER_ID);
	}

	@AfterEach
	void afterEach() throws Exception {
		StateUtil.removeSessions(USER_ID);
		DefaultCaching.get().shutdown();
		ProvidedRepositoryFactory.set(originalRepository);
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
		UtilImpl.SESSION_CACHE = originalSessionCache;

		if (cacheDirectory != null) {
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
					.map(Path::toFile)
					.forEach(File::delete);
			}
		}
	}

	@Test
	void shouldLogConcurrentSessionWarningOnlyWhenAllConditionsAreMet() {
		assertTrue(WebUtil.shouldLogConcurrentSessionWarning(true, false, true, true));
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(false, false, true, true));
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(true, true, true, true));
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(true, false, false, true));
		assertFalse(WebUtil.shouldLogConcurrentSessionWarning(true, false, true, false));
	}

	@Test
	void buildConcurrentSessionWarningMessageIncludesExistingSessionCount() {
		assertEquals("User logged in while another active session already existed. Existing session count: 2.",
						WebUtil.buildConcurrentSessionWarningMessage(2));
	}

	@Test
	void isPublicUserMatchesConfiguredPublicUserName() {
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn("demo");
		when(user.getName()).thenReturn("public");

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.retrievePublicUserName("demo")).thenReturn("public");
		assertTrue(WebUtil.isPublicUser(user, repository));

		when(repository.retrievePublicUserName("demo")).thenReturn("someoneElse");
		assertFalse(WebUtil.isPublicUser(user, repository));
	}

	@Test
	void isPublicUserReturnsFalseWhenCustomerNameIsNull() {
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn(null);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		assertFalse(WebUtil.isPublicUser(user, repository));
	}

	@Test
	void concurrentSessionWarningEligibilityRequiresPrincipalAndNonPublicUser() {
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn("demo");
		when(user.getName()).thenReturn("alice");

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.retrievePublicUserName("demo")).thenReturn("public");
		Principal principal = () -> "demo/alice";

		assertTrue(WebUtil.isConcurrentSessionWarningEligible(user, principal, repository));
		assertFalse(WebUtil.isConcurrentSessionWarningEligible(user, null, repository));

		when(user.getName()).thenReturn("public");
		assertFalse(WebUtil.isConcurrentSessionWarningEligible(user, principal, repository));
	}

	@Test
	void concurrentSessionWarningEligibilityUsesRequestPrincipalAndRepositoryFactory() {
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn("demo");
		when(user.getName()).thenReturn("alice");

		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.retrievePublicUserName("demo")).thenReturn("public");
		ProvidedRepositoryFactory.set(repository);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getUserPrincipal()).thenReturn(() -> "demo/alice");

		assertTrue(WebUtil.isConcurrentSessionWarningEligible(user, request));
	}

	@Test
	void addSessionAndAuditConcurrentSessionWarningAddsSessionWithoutLoggingWhenWarningsDisabled() {
		boolean originalWarnings = UtilImpl.CONCURRENT_SESSION_WARNINGS;
		try {
			UtilImpl.CONCURRENT_SESSION_WARNINGS = false;
			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.retrievePublicUserName("demo")).thenReturn("public");
			ProvidedRepositoryFactory.set(repository);

			User user = user("demo", "alice", USER_ID);
			HttpSession session = session("A");
			HttpServletRequest request = requestWithPrincipal("demo/alice");

			WebUtil.addSessionAndAuditConcurrentSessionWarning(user, request, session);

			assertEquals(1, StateUtil.getSessionCount(USER_ID));
			assertTrue(StateUtil.checkSession(USER_ID, session));
		}
		finally {
			UtilImpl.CONCURRENT_SESSION_WARNINGS = originalWarnings;
		}
	}

	@Test
	void addSessionAndAuditConcurrentSessionWarningHandlesEligibilityFailures() {
		boolean originalWarnings = UtilImpl.CONCURRENT_SESSION_WARNINGS;
		try {
			UtilImpl.CONCURRENT_SESSION_WARNINGS = false;
			ProvidedRepositoryFactory.set(mock(ProvidedRepository.class));
			User user = user("demo", "alice", USER_ID);
			HttpSession existingSession = session("A");
			HttpSession currentSession = session("B");
			StateUtil.addSession(USER_ID, existingSession);

			HttpServletRequest request = mock(HttpServletRequest.class);
			when(request.getUserPrincipal()).thenReturn(() -> "demo/alice");
			when(user.getCustomerName()).thenThrow(new IllegalStateException("boom"));

			WebUtil.addSessionAndAuditConcurrentSessionWarning(user, request, currentSession);

			assertEquals(2, StateUtil.getSessionCount(USER_ID));
			assertTrue(StateUtil.checkSession(USER_ID, currentSession));
		}
		finally {
			UtilImpl.CONCURRENT_SESSION_WARNINGS = originalWarnings;
		}
	}

	private static User user(String customerName, String userName, String userId) {
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn(customerName);
		when(user.getName()).thenReturn(userName);
		when(user.getId()).thenReturn(userId);
		return user;
	}

	private static HttpSession session(String id) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn(id);
		return session;
	}

	private static HttpServletRequest requestWithPrincipal(String principalName) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getUserPrincipal()).thenReturn(() -> principalName);
		return request;
	}
}
