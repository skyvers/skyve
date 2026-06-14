package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

import jakarta.servlet.http.HttpSession;
import jakarta.servlet.http.HttpSessionEvent;

@SuppressWarnings("static-method")
class SkyveSessionListenerTest {
	private static final String USER_ID = "user-1";

	private ProvidedRepository originalRepository;
	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private Path cacheDirectory;

	@BeforeEach
	void setUp() throws Exception {
		originalRepository = ProvidedRepositoryFactory.get();
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;

		cacheDirectory = Files.createTempDirectory("skyve-session-listener-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);

		DefaultCaching.get().shutdown();
		DefaultCaching.get().startup();
		StateUtil.removeSessions(USER_ID);
		while (StateUtil.getSessionCount() > 0) {
			StateUtil.decrementSessionCount();
		}
	}

	@AfterEach
	void tearDown() throws Exception {
		StateUtil.removeSessions(USER_ID);
		DefaultCaching.get().shutdown();
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
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
	void sessionCreatedIncrementsCount() {
		SkyveSessionListener listener = new SkyveSessionListener();
		HttpSession session = mock(HttpSession.class);
		HttpSessionEvent event = new HttpSessionEvent(session);

		assertEquals(0, StateUtil.getSessionCount());
		listener.sessionCreated(event);
		assertEquals(1, StateUtil.getSessionCount());
	}

	@Test
	void sessionDestroyedWithoutSkyveUserDecrementsAndClampsToZero() {
		SkyveSessionListener listener = new SkyveSessionListener();
		HttpSession session = mock(HttpSession.class);
		when(session.getAttribute(org.skyve.web.WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		HttpSessionEvent event = new HttpSessionEvent(session);

		listener.sessionDestroyed(event);

		assertEquals(0, StateUtil.getSessionCount());
	}

	@Test
	void sessionDestroyedWithSkyveUserWithoutCustomerNameRemovesSessionAndDecrements() {
		StateUtil.incrementSessionCount();
		SkyveSessionListener listener = new SkyveSessionListener();
		User user = mock(User.class);
		HttpSession session = mock(HttpSession.class);
		when(user.getId()).thenReturn(USER_ID);
		when(user.getCustomerName()).thenReturn(null);
		when(session.getAttribute(org.skyve.web.WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		when(session.getId()).thenReturn("session-1");
		HttpSessionEvent event = new HttpSessionEvent(session);

		listener.sessionDestroyed(event);

		assertEquals(0, StateUtil.getSessionCount());
	}

	@Test
	void sessionDestroyedWithSkyveUserNotifiesCustomerLogout() {
		StateUtil.incrementSessionCount();
		SkyveSessionListener listener = new SkyveSessionListener();
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		HttpSession session = mock(HttpSession.class);
		when(user.getId()).thenReturn(USER_ID);
		when(user.getCustomerName()).thenReturn("demo");
		when(user.getCustomer()).thenReturn(customer);
		when(session.getAttribute(org.skyve.web.WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		when(session.getId()).thenReturn("session-1");
		HttpSessionEvent event = new HttpSessionEvent(session);

		listener.sessionDestroyed(event);

		assertEquals(0, StateUtil.getSessionCount());
		verify(customer).notifyLogout(user, session);
	}
}
