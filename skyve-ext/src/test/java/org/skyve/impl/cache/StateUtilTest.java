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
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.util.UtilImpl;

import jakarta.servlet.http.HttpSession;

/**
 * Unit tests for user-session tracking helpers in {@link StateUtil}.
 */
public class StateUtilTest {
	private static final String USER_ID = "test-user";

	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private Path cacheDirectory;

	@Before
	public void before() throws Exception {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;

		cacheDirectory = Files.createTempDirectory("skyve-stateutil-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);

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

		if (cacheDirectory != null) {
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
					.map(Path::toFile)
					.forEach(File::delete);
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

	private static HttpSession session(String id) {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn(id);
		return session;
	}
}
