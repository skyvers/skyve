package org.skyve.impl.cache;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.Serializable;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.CacheExpiryPolicy;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.web.UserAgentType;

// Global configuration snapshots must remain per-test-instance; repeated values are cache fixtures.
@SuppressWarnings({ "static-method", "java:S1170", "java:S1192" })
class DefaultCachingTest {
	@TempDir
	private Path cacheDirectory;

	private final boolean originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
	private final String originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
	private final boolean originalCacheMultiple = UtilImpl.CACHE_MULTIPLE;
	private final ConversationCacheConfig originalConversationCache = UtilImpl.CONVERSATION_CACHE;
	private final CSRFTokenCacheConfig originalCsrfTokenCache = UtilImpl.CSRF_TOKEN_CACHE;
	private final SessionCacheConfig originalSessionCache = UtilImpl.SESSION_CACHE;
	private final GeoIPCacheConfig originalGeoIPCache = UtilImpl.GEO_IP_CACHE;
	private final List<HibernateCacheConfig> originalHibernateCaches = UtilImpl.HIBERNATE_CACHES;
	private final List<CacheConfig<? extends Serializable, ? extends Serializable>> originalAppCaches = UtilImpl.APP_CACHES;

	@AfterEach
	void tearDown() {
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
		UtilImpl.CACHE_MULTIPLE = originalCacheMultiple;
		UtilImpl.CONVERSATION_CACHE = originalConversationCache;
		UtilImpl.CSRF_TOKEN_CACHE = originalCsrfTokenCache;
		UtilImpl.SESSION_CACHE = originalSessionCache;
		UtilImpl.GEO_IP_CACHE = originalGeoIPCache;
		UtilImpl.HIBERNATE_CACHES = originalHibernateCaches;
		UtilImpl.APP_CACHES = originalAppCaches;
	}

	@Test
	void getReturnsSingleton() {
		assertSame(DefaultCaching.get(), DefaultCaching.get());
	}

	@Test
	void accessBeforeStartupThrowsDomainException() {
		DefaultCaching caching = DefaultCaching.get();
		caching.shutdown();

		assertThrows(DomainException.class, caching::getEHCacheManager);
		assertThrows(DomainException.class, caching::getJCacheManager);
		assertThrows(DomainException.class, () -> caching.isEHCache("missing-cache"));
		assertThrows(DomainException.class, () -> caching.isJCache("missing-cache"));
		assertThrows(DomainException.class,
				() -> caching.getEHCache("missing-cache", String.class, String.class));
		assertThrows(DomainException.class,
				() -> caching.getJCache("missing-cache", String.class, String.class));
		assertThrows(DomainException.class, () -> caching.getEHCacheStatistics("missing-cache"));
	}

	@Test
	void startupRejectsMultipleCacheInstancesWhenAnAppCacheIsPersistent() {
		DefaultCaching.get().shutdown();
		UtilImpl.CACHE_MULTIPLE = true;
		UtilImpl.APP_CACHES = new ArrayList<>();
		UtilImpl.APP_CACHES.add(new EHCacheConfig<>("persistentAppCache",
														10L,
														0L,
														CacheExpiryPolicy.eternal,
														0L,
														String.class,
														String.class,
														1L,
														true));

		assertThrows(IllegalStateException.class, DefaultCaching.get()::startup);
	}

	@Test
	@SuppressWarnings("resource")
	void startupCreatesConfiguredEHCacheJCacheAndHibernateRegions() {
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.CACHE_MULTIPLE = false;
		UtilImpl.CONVERSATION_CACHE = null;
		UtilImpl.CSRF_TOKEN_CACHE = null;
		UtilImpl.SESSION_CACHE = null;
		UtilImpl.GEO_IP_CACHE = null;
		UtilImpl.APP_CACHES = new ArrayList<>();
		UtilImpl.HIBERNATE_CACHES = new ArrayList<>();
		UtilImpl.APP_CACHES.add(new EHCacheConfig<>("defaultCachingEh",
														10L,
														CacheExpiryPolicy.timeToLive,
														5L,
														String.class,
														String.class));
		UtilImpl.APP_CACHES.add(new JCacheConfig<>("defaultCachingJ",
														10L,
														CacheExpiryPolicy.timeToIdle,
														5L,
														String.class,
														String.class));
		UtilImpl.HIBERNATE_CACHES.add(new HibernateCacheConfig("defaultCachingHibernate",
																10L,
																CacheExpiryPolicy.eternal,
																0L));

		DefaultCaching caching = DefaultCaching.get();
		caching.startup();
		caching.startup();

		assertNotNull(caching.getEHCacheManager());
		assertNotNull(caching.getJCacheManager());
		assertNotNull(caching.getEHCache(DefaultCaching.USER_AGENT_TYPE_CACHE_NAME,
											String.class,
											UserAgentType.class));
		assertNotNull(caching.getEHCache("defaultCachingEh", String.class, String.class));
		assertNotNull(caching.getJCache("defaultCachingJ", String.class, String.class));
		assertNotNull(caching.getJCache("defaultCachingHibernate", Serializable.class, Serializable.class));
		assertTrue(caching.isEHCache("defaultCachingEh"));
		assertFalse(caching.isEHCache("missing-cache"));
		assertTrue(caching.isJCache("defaultCachingJ"));
		assertFalse(caching.isJCache("missing-cache"));
		assertThrows(DomainException.class, () -> caching.getEHCacheStatistics("missing-cache"));
		assertNull(caching.getEHTierStatistics(caching.getEHCacheStatistics(DefaultCaching.USER_AGENT_TYPE_CACHE_NAME),
				org.skyve.cache.CacheTier.Disk));
		assertNull(caching.getJCacheStatisticsMXBean("missing-cache"));

		caching.removeEHCache("defaultCachingEh");
		assertFalse(caching.isEHCache("defaultCachingEh"));
		assertThrows(DomainException.class,
				() -> caching.getEHCache("defaultCachingEh", String.class, String.class));
		caching.destroyJCache("defaultCachingJ");
		assertFalse(caching.isJCache("defaultCachingJ"));
		assertThrows(DomainException.class,
				() -> caching.getJCache("defaultCachingJ", String.class, String.class));
	}
}
