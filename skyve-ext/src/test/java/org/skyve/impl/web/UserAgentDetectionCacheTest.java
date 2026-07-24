package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.ehcache.Cache;
import org.ehcache.core.statistics.CacheStatistics;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.util.UtilImpl;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class UserAgentDetectionCacheTest {
	private static final int EXPECTED_CAPACITY = (int) DefaultCaching.USER_AGENT_TYPE_CACHE_CAPACITY;
	private static boolean originalForceNonPersistentCaching;
	private static boolean originalCacheMultiple;
	private static ConversationCacheConfig originalConversationCache;
	private static CSRFTokenCacheConfig originalCsrfTokenCache;
	private static SessionCacheConfig originalSessionCache;
	private static GeoIPCacheConfig originalGeoIpCache;
	private static List<HibernateCacheConfig> originalHibernateCaches;
	private static List<CacheConfig<? extends Serializable, ? extends Serializable>> originalAppCaches;

	@BeforeAll
	static void startCacheManager() {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheMultiple = UtilImpl.CACHE_MULTIPLE;
		originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		originalCsrfTokenCache = UtilImpl.CSRF_TOKEN_CACHE;
		originalSessionCache = UtilImpl.SESSION_CACHE;
		originalGeoIpCache = UtilImpl.GEO_IP_CACHE;
		originalHibernateCaches = UtilImpl.HIBERNATE_CACHES;
		originalAppCaches = UtilImpl.APP_CACHES;

		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_MULTIPLE = false;
		UtilImpl.CONVERSATION_CACHE = null;
		UtilImpl.CSRF_TOKEN_CACHE = null;
		UtilImpl.SESSION_CACHE = null;
		UtilImpl.GEO_IP_CACHE = null;
		UtilImpl.HIBERNATE_CACHES = new ArrayList<>();
		UtilImpl.APP_CACHES = new ArrayList<>();
		DefaultCaching.get().startup();
	}

	@AfterAll
	static void stopCacheManager() {
		DefaultCaching.get().shutdown();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_MULTIPLE = originalCacheMultiple;
		UtilImpl.CONVERSATION_CACHE = originalConversationCache;
		UtilImpl.CSRF_TOKEN_CACHE = originalCsrfTokenCache;
		UtilImpl.SESSION_CACHE = originalSessionCache;
		UtilImpl.GEO_IP_CACHE = originalGeoIpCache;
		UtilImpl.HIBERNATE_CACHES = originalHibernateCaches;
		UtilImpl.APP_CACHES = originalAppCaches;
	}

	@BeforeEach
	void clearCache() {
		cache().clear();
	}

	@Test
	void concurrentRepeatedAndDistinctHeadersRemainSafeAndBounded() throws Exception {
		ExecutorService executor = Executors.newFixedThreadPool(12);
		try {
			List<Callable<UserAgentType>> calls = new ArrayList<>(1200);
			for (int i = 0; i < 1200; i++) {
				String header = (i % 4 == 0) ? "shared-concurrent-agent" : "distinct-concurrent-agent-" + i;
				calls.add(() -> UserAgent.detectType(request(header)));
			}
			List<Future<UserAgentType>> results = executor.invokeAll(calls);
			for (Future<UserAgentType> result : results) {
				assertEquals(UserAgentType.other, result.get());
			}
		}
		finally {
			executor.shutdownNow();
		}

		assertTrue(cacheSize() <= EXPECTED_CAPACITY);
	}

	@Test
	void cacheStaysBoundedAndEvictsWhenFull() {
		for (int i = 0; i < EXPECTED_CAPACITY + 512; i++) {
			cache().put("agent-" + i, UserAgentType.other);
		}

		assertTrue(cacheSize() <= EXPECTED_CAPACITY);
		assertTrue(statistics().getCacheEvictions() > 0L);
	}

	@Test
	void evictionDoesNotChangeDetectionSemantics() {
		String mobileHeader = "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) " +
				"AppleWebKit/605.1.15 Version/17.0 Mobile/15E148 Safari/604.1";
		assertEquals(UserAgentType.phone, UserAgent.detectType(request(mobileHeader)));

		for (int i = 0; i < EXPECTED_CAPACITY * 2; i++) {
			cache().put("eviction-agent-" + i, UserAgentType.other);
		}
		assertTrue(statistics().getCacheEvictions() > 0L);
		assertEquals(UserAgentType.phone, UserAgent.detectType(request(mobileHeader)));
	}

	private static HttpServletRequest request(String userAgent) {
		Map<String, Object> attributes = new java.util.concurrent.ConcurrentHashMap<>();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getAttribute(any())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		when(request.getHeader("User-Agent")).thenReturn(userAgent);
		org.mockito.Mockito.doAnswer(invocation -> {
			attributes.put(invocation.getArgument(0), invocation.getArgument(1));
			return null;
		}).when(request).setAttribute(any(), any());
		return request;
	}

	private static int cacheSize() {
		int result = 0;
		for (@SuppressWarnings("unused") Cache.Entry<String, UserAgentType> ignored : cache()) {
			result++;
		}
		return result;
	}

	private static @Nonnull Cache<String, UserAgentType> cache() {
		return EXT.getCaching().getEHCache(DefaultCaching.USER_AGENT_TYPE_CACHE_NAME,
															String.class,
															UserAgentType.class);
	}

	private static @Nonnull CacheStatistics statistics() {
		return EXT.getCaching().getEHCacheStatistics(DefaultCaching.USER_AGENT_TYPE_CACHE_NAME);
	}
}
