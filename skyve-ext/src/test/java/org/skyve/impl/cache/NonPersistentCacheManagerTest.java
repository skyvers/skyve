package org.skyve.impl.cache;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.ehcache.Cache;
import org.ehcache.CacheManager;
import org.ehcache.Status;
import org.ehcache.config.Builder;
import org.ehcache.config.CacheConfiguration;
import org.ehcache.config.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("resource")
class NonPersistentCacheManagerTest {

	private CacheManager delegate;
	private NonPersistentCacheManager cacheManager;

	@BeforeEach
	void setUp() {
		delegate = mock(CacheManager.class);
		cacheManager = new NonPersistentCacheManager(delegate);
	}

	@Test
	void createCacheWithBuilderDelegatesToDelegate() {
		Builder<CacheConfiguration<String, String>> builder = mock(Builder.class);
		Cache<String, String> expected = mock(Cache.class);
		when(delegate.createCache("test", builder)).thenReturn(expected);

		Cache<String, String> result = cacheManager.createCache("test", builder);

		verify(delegate).createCache("test", builder);
		assertEquals(expected, result);
	}

	@Test
	void removeCacheDelegatesToDelegate() {
		cacheManager.removeCache("myCache");
		verify(delegate).removeCache("myCache");
	}

	@Test
	void initDelegatesToDelegate() {
		cacheManager.init();
		verify(delegate).init();
	}

	@Test
	void closeDelegatesToDelegate() {
		cacheManager.close();
		verify(delegate).close();
	}

	@Test
	void getStatusDelegatesToDelegate() {
		when(delegate.getStatus()).thenReturn(Status.AVAILABLE);
		Status result = cacheManager.getStatus();
		assertEquals(Status.AVAILABLE, result);
		verify(delegate).getStatus();
	}

	@Test
	void getRuntimeConfigurationDelegatesToDelegate() {
		Configuration config = mock(Configuration.class);
		when(delegate.getRuntimeConfiguration()).thenReturn(config);

		Configuration result = cacheManager.getRuntimeConfiguration();

		assertEquals(config, result);
		verify(delegate).getRuntimeConfiguration();
	}

	@Test
	void destroyIsNoOp() {
		// destroy() is a no-op in NonPersistentCacheManager — just verify it does not throw
		assertDoesNotThrow(cacheManager::destroy);
	}

	@Test
	void destroyCacheIsNoOp() {
		// destroyCache() is a no-op — just verify it does not throw
		assertDoesNotThrow(() -> cacheManager.destroyCache("any"));
	}

	@Test
	void getCacheDelegatesToDelegate() {
		Cache<String, Integer> expected = mock(Cache.class);
		when(delegate.getCache("myCache", String.class, Integer.class)).thenReturn(expected);

		Cache<String, Integer> result = cacheManager.getCache("myCache", String.class, Integer.class);

		assertEquals(expected, result);
		verify(delegate).getCache("myCache", String.class, Integer.class);
	}

	@Test
	void createCacheWithConfigDelegatesToDelegate() {
		CacheConfiguration<String, String> config = mock(CacheConfiguration.class);
		Cache<String, String> expected = mock(Cache.class);
		when(delegate.createCache("test", config)).thenReturn(expected);

		Cache<String, String> result = cacheManager.createCache("test", config);

		verify(delegate).createCache("test", config);
		assertEquals(expected, result);
	}
}
