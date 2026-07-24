package org.skyve.impl.cdi;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Map;

import org.ehcache.Cache;
import org.ehcache.PersistentCacheManager;
import org.ehcache.Status;
import org.ehcache.config.CacheConfiguration;
import org.ehcache.config.Configuration;
import org.ehcache.core.spi.service.StatisticsService;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.junit.jupiter.api.Test;
import org.skyve.cache.CacheTier;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.impl.cache.DefaultCaching;

@SuppressWarnings({ "static-method", "unchecked", "resource", "boxing" })
class CachingInjectableTest {
	@Test
	void delegatesCachingOperationsToDefaultCachingSingleton() throws Exception {
		DefaultCaching caching = DefaultCaching.get();
		PersistentCacheManager ehManager = mock(PersistentCacheManager.class);
		javax.cache.CacheManager jManager = mock(javax.cache.CacheManager.class);
		Configuration ehRuntimeConfiguration = mock(Configuration.class);
		StatisticsService statisticsService = mock(StatisticsService.class);
		Cache<String, String> ehCache = mock(Cache.class);
		javax.cache.Cache<String, String> jCache = mock(javax.cache.Cache.class);
		CacheStatistics ehStats = mock(CacheStatistics.class);
		TierStatistics tierStats = mock(TierStatistics.class);
		Map<String, TierStatistics> tierStatsMap = Map.of(CacheTier.OnHeap.toString(), tierStats);
		EHCacheConfig<String, String> ehConfig = new EHCacheConfig<>("ehCreate", 10, String.class, String.class);
		JCacheConfig<String, String> jConfig = new JCacheConfig<>("jCreate", 10, String.class, String.class);
		CachingInjectable injectable = new CachingInjectable();

		Object originalEhManager = getField(caching, "ehCacheManager");
		Object originalJManager = getField(caching, "jCacheManager");
		Object originalStatistics = getField(caching, "statisticsService");
		try {
			setField(caching, "ehCacheManager", ehManager);
			setField(caching, "jCacheManager", jManager);
			setField(caching, "statisticsService", statisticsService);

			when(ehManager.getCache("eh", String.class, String.class)).thenReturn(ehCache);
			when(jManager.getCache("j", String.class, String.class)).thenReturn(jCache);
			when(ehManager.getRuntimeConfiguration()).thenReturn(ehRuntimeConfiguration);
			when(ehRuntimeConfiguration.getCacheConfigurations()).thenReturn(Map.of("eh", mock(CacheConfiguration.class)));
			when(jManager.<String, String> getCache("j")).thenReturn(jCache);
			when(ehManager.createCache(eq("ehCreate"), any(CacheConfiguration.class))).thenReturn(ehCache);
			when(jManager.createCache(eq("jCreate"), any()))
					.thenReturn((javax.cache.Cache<Object, Object>) (javax.cache.Cache<?, ?>) jCache);
			when(ehManager.getStatus()).thenReturn(Status.AVAILABLE);
			when(jManager.isClosed()).thenReturn(false);
			when(statisticsService.getCacheStatistics("eh")).thenReturn(ehStats);
			when(ehStats.getTierStatistics()).thenReturn(tierStatsMap);

			injectable.startup();

			assertTrue(injectable.isEHCache("eh"));
			assertTrue(injectable.isJCache("j"));
			assertSame(ehCache, injectable.getEHCache("eh", String.class, String.class));
			assertSame(jCache, injectable.getJCache("j", String.class, String.class));
			assertSame(ehCache, injectable.createEHCache(ehConfig));
			assertSame(jCache, injectable.createJCache(jConfig));
			injectable.removeEHCache("eh");
			injectable.destroyEHCache("eh");
			injectable.destroyJCache("j");
			assertSame(ehStats, injectable.getEHCacheStatistics("eh"));
			assertSame(tierStats, injectable.getEHTierStatistics(ehStats, CacheTier.OnHeap));
			assertNull(injectable.getEHTierStatistics(ehStats, CacheTier.Disk));
			assertNull(injectable.getJCacheStatisticsMXBean("no-such-cache"));
			assertSame(ehManager, injectable.getEHCacheManager());
			assertSame(jManager, injectable.getJCacheManager());
			injectable.shutdown();
		}
		finally {
			setField(caching, "ehCacheManager", originalEhManager);
			setField(caching, "jCacheManager", originalJManager);
			setField(caching, "statisticsService", originalStatistics);
		}

		verify(ehManager).removeCache("eh");
		verify(ehManager).destroyCache("eh");
		verify(jManager).destroyCache("j");
		verify(ehManager).close();
		verify(jManager).close();
	}

	private static Object getField(Object target, String name) throws Exception {
		Field field = DefaultCaching.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = DefaultCaching.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
