package org.skyve.cache;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class CacheTest {

	// --- CacheExpiryPolicy ---

	@Test
	void cacheExpiryPolicyTimeToLiveValue() {
		assertThat(CacheExpiryPolicy.valueOf("timeToLive"), is(CacheExpiryPolicy.timeToLive));
	}

	@Test
	void cacheExpiryPolicyTimeToIdleValue() {
		assertThat(CacheExpiryPolicy.valueOf("timeToIdle"), is(CacheExpiryPolicy.timeToIdle));
	}

	@Test
	void cacheExpiryPolicyEternalValue() {
		assertThat(CacheExpiryPolicy.valueOf("eternal"), is(CacheExpiryPolicy.eternal));
	}

	// --- EHCacheConfig ---

	@Test
	void ehCacheConfigFullConstructorStoresAllValues() {
		EHCacheConfig<String, String> config = new EHCacheConfig<>(
			"testCache",
			1000L,
			256L,
			CacheExpiryPolicy.timeToLive,
			30L,
			String.class,
			String.class,
			512L
		);
		assertThat(config.getName(), is("testCache"));
		assertEquals(1000L, config.getHeapSizeEntries());
		assertEquals(256L, config.getOffHeapSizeInMB());
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.timeToLive));
		assertEquals(30L, config.getExpiryInMinutes());
		assertThat(config.getKeyClass().getName(), is(String.class.getName()));
		assertThat(config.getValueClass().getName(), is(String.class.getName()));
		assertEquals(512L, config.getDiskSizeInMB());
	}

	@Test
	void ehCacheConfigSimpleConstructorDefaults() {
		EHCacheConfig<String, String> config = new EHCacheConfig<>(
			"simple",
			500L,
			String.class,
			String.class
		);
		assertThat(config.getName(), is("simple"));
		assertEquals(500L, config.getHeapSizeEntries());
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.eternal));
		assertEquals(0L, config.getExpiryInMinutes());
		assertEquals(0L, config.getDiskSizeInMB());
		assertFalse(config.isPersistent());
	}

	@Test
	void ehCacheConfigWithExpiryConstructor() {
		EHCacheConfig<String, String> config = new EHCacheConfig<>(
			"expiring",
			200L,
			CacheExpiryPolicy.timeToIdle,
			60L,
			String.class,
			String.class
		);
		assertThat(config.getName(), is("expiring"));
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.timeToIdle));
		assertEquals(60L, config.getExpiryInMinutes());
	}

	@Test
	void ehCacheConfigWithDiskSizeAndExpiryConstructor() {
		EHCacheConfig<String, String> config = new EHCacheConfig<>(
			"diskCache",
			100L,
			CacheExpiryPolicy.timeToLive,
			15L,
			String.class,
			String.class,
			1024L
		);
		assertEquals(1024L, config.getDiskSizeInMB());
		assertFalse(config.isPersistent());
	}

	@Test
	void ehCacheConfigPersistentConstructor() {
		EHCacheConfig<String, String> config = new EHCacheConfig<>(
			"persistentCache",
			100L,
			CacheExpiryPolicy.timeToLive,
			60L,
			String.class,
			String.class,
			512L,
			true
		);
		assertTrue(config.isPersistent());
	}

	// --- HibernateCacheConfig ---

	@Test
	void hibernateCacheConfigBasicConstructor() {
		HibernateCacheConfig config = new HibernateCacheConfig("hibernateCache", 500L);
		assertThat(config.getName(), is("hibernateCache"));
		assertEquals(500L, config.getHeapSizeEntries());
		assertThat(config.toString(), notNullValue());
	}

	@Test
	void hibernateCacheConfigWithExpiryConstructor() {
		HibernateCacheConfig config = new HibernateCacheConfig(
			"hCache",
			1000L,
			CacheExpiryPolicy.timeToLive,
			120L
		);
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.timeToLive));
		assertEquals(120L, config.getExpiryInMinutes());
	}

	@Test
	void hibernateCacheConfigWithOffHeapConstructor() {
		HibernateCacheConfig config = new HibernateCacheConfig("hCache2", 200L, 128L);
		assertEquals(128L, config.getOffHeapSizeInMB());
	}

	@Test
	void hibernateCacheConfigToStringContainsName() {
		HibernateCacheConfig config = new HibernateCacheConfig("myHibernateCache", 100L);
		assertTrue(config.toString().contains("myHibernateCache"));
	}

	// --- SessionCacheConfig ---

	@Test
	void sessionCacheConfigBasicConstructor() {
		SessionCacheConfig config = new SessionCacheConfig(200L, 30L);
		assertThat(config.getName(), is("sessions"));
		assertEquals(200L, config.getHeapSizeEntries());
		assertEquals(30L, config.getExpiryInMinutes());
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.timeToIdle));
		assertThat(config.getKeyClass().getName(), is(String.class.getName()));
	}

	@Test
	void sessionCacheConfigWithDiskConstructor() {
		SessionCacheConfig config = new SessionCacheConfig(100L, 512L, 60L);
		assertEquals(100L, config.getHeapSizeEntries());
		assertEquals(512L, config.getDiskSizeInMB());
		assertEquals(60L, config.getExpiryInMinutes());
	}

	@Test
	void sessionCacheConfigWithOffHeapAndDiskConstructor() {
		SessionCacheConfig config = new SessionCacheConfig(50L, 256L, 1024L, 45L);
		assertEquals(256L, config.getOffHeapSizeInMB());
		assertEquals(1024L, config.getDiskSizeInMB());
		assertEquals(45L, config.getExpiryInMinutes());
	}

	@Test
	void sessionCacheConfigToStringNotNull() {
		SessionCacheConfig config = new SessionCacheConfig(100L, 30L);
		assertThat(config.toString(), notNullValue());
	}

	@Test
	void sessionCacheConfigNameIsAlwaysSessions() {
		SessionCacheConfig config = new SessionCacheConfig(100L, 30L);
		assertThat(config.getName(), is("sessions"));
	}
}
