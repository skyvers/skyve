package org.skyve.cache;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class EHCacheConfigTest {

	@Test
	@SuppressWarnings("static-method")
	void heapOnlyConstructorSetsDefaults() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("myCache", 1000L, String.class, String.class);
		assertThat(cfg.getName(), is("myCache"));
		assertEquals(1000L, cfg.getHeapSizeEntries());
		assertEquals(0L, cfg.getDiskSizeInMB());
		assertFalse(cfg.isPersistent());
	}

	@Test
	@SuppressWarnings("static-method")
	void heapWithDiskConstructorSetsDiskSize() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache2", 500L, String.class, String.class, 128L);
		assertEquals(128L, cfg.getDiskSizeInMB());
		assertFalse(cfg.isPersistent());
	}

	@Test
	@SuppressWarnings("static-method")
	void persistentConstructorSetsPersistent() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache3", 500L, String.class, String.class, 128L, true);
		assertTrue(cfg.isPersistent());
	}

	@Test
	@SuppressWarnings("static-method")
	void expiryConstructorSetsExpiryPolicy() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache4", 200L, CacheExpiryPolicy.timeToIdle, 30L, String.class, String.class);
		assertThat(cfg.getExpiryPolicy(), is(CacheExpiryPolicy.timeToIdle));
		assertEquals(30L, cfg.getExpiryInMinutes());
	}

	@Test
	@SuppressWarnings("static-method")
	void expiryWithDiskConstructorSetsDiskSize() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache5", 200L, CacheExpiryPolicy.timeToLive, 60L, String.class, String.class, 64L);
		assertEquals(64L, cfg.getDiskSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	void expiryPersistentConstructorSetsPersistent() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache6", 200L, CacheExpiryPolicy.timeToLive, 60L, String.class, String.class, 32L, true);
		assertTrue(cfg.isPersistent());
	}

	@Test
	@SuppressWarnings("static-method")
	void offHeapConstructorSetsOffHeapSize() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache7", 100L, 256L, String.class, String.class);
		assertEquals(256L, cfg.getOffHeapSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	void offHeapWithDiskConstructorSetsBoth() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache8", 100L, 256L, String.class, String.class, 128L);
		assertEquals(256L, cfg.getOffHeapSizeInMB());
		assertEquals(128L, cfg.getDiskSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	void offHeapPersistentConstructorSetsPersistent() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cache9", 100L, 256L, String.class, String.class, 128L, true);
		assertTrue(cfg.isPersistent());
	}

	@Test
	@SuppressWarnings("static-method")
	void fullExpiryOffHeapConstructorSetsAll() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cacheA", 50L, 128L, CacheExpiryPolicy.timeToIdle, 15L, String.class, String.class, 32L);
		assertEquals(128L, cfg.getOffHeapSizeInMB());
		assertEquals(32L, cfg.getDiskSizeInMB());
		assertThat(cfg.getExpiryPolicy(), is(CacheExpiryPolicy.timeToIdle));
	}

	@Test
	@SuppressWarnings("static-method")
	void fullExpiryOffHeapPersistentConstructorSetsPersistent() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("cacheB", 50L, 128L, CacheExpiryPolicy.timeToLive, 20L, String.class, String.class, 32L, true);
		assertTrue(cfg.isPersistent());
	}

	@Test
	@SuppressWarnings("static-method")
	void toStringContainsName() {
		EHCacheConfig<String, String> cfg = new EHCacheConfig<>("mySpecialCache", 100L, String.class, String.class, 64L);
		String str = cfg.toString();
		assertTrue(str.contains("mySpecialCache"), "toString should contain cache name: " + str);
		assertTrue(str.contains("ehcache"), "toString should contain 'ehcache': " + str);
	}
}
