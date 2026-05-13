package org.skyve.cache;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(config.getHeapSizeEntries(), is(1000L));
		assertThat(config.getOffHeapSizeInMB(), is(256L));
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.timeToLive));
		assertThat(config.getExpiryInMinutes(), is(30L));
		assertThat(config.getKeyClass().getName(), is(String.class.getName()));
		assertThat(config.getValueClass().getName(), is(String.class.getName()));
		assertThat(config.getDiskSizeInMB(), is(512L));
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
		assertThat(config.getHeapSizeEntries(), is(500L));
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.eternal));
		assertThat(config.getExpiryInMinutes(), is(0L));
		assertThat(config.getDiskSizeInMB(), is(0L));
		assertThat(config.isPersistent(), is(false));
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
		assertThat(config.getExpiryInMinutes(), is(60L));
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
		assertThat(config.getDiskSizeInMB(), is(1024L));
		assertThat(config.isPersistent(), is(false));
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
		assertThat(config.isPersistent(), is(true));
	}

	// --- HibernateCacheConfig ---

	@Test
	void hibernateCacheConfigBasicConstructor() {
		HibernateCacheConfig config = new HibernateCacheConfig("hibernateCache", 500L);
		assertThat(config.getName(), is("hibernateCache"));
		assertThat(config.getHeapSizeEntries(), is(500L));
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
		assertThat(config.getExpiryInMinutes(), is(120L));
	}

	@Test
	void hibernateCacheConfigWithOffHeapConstructor() {
		HibernateCacheConfig config = new HibernateCacheConfig("hCache2", 200L, 128L);
		assertThat(config.getOffHeapSizeInMB(), is(128L));
	}

	@Test
	void hibernateCacheConfigToStringContainsName() {
		HibernateCacheConfig config = new HibernateCacheConfig("myHibernateCache", 100L);
		assertThat(config.toString().contains("myHibernateCache"), is(true));
	}

	// --- SessionCacheConfig ---

	@SuppressWarnings("rawtypes")
	@Test
	void sessionCacheConfigBasicConstructor() {
		SessionCacheConfig config = new SessionCacheConfig(200L, 30L);
		assertThat(config.getName(), is("sessions"));
		assertThat(config.getHeapSizeEntries(), is(200L));
		assertThat(config.getExpiryInMinutes(), is(30L));
		assertThat(config.getExpiryPolicy(), is(CacheExpiryPolicy.timeToIdle));
		assertThat(config.getKeyClass().getName(), is(String.class.getName()));
	}

	@SuppressWarnings("rawtypes")
	@Test
	void sessionCacheConfigWithDiskConstructor() {
		SessionCacheConfig config = new SessionCacheConfig(100L, 512L, 60L);
		assertThat(config.getHeapSizeEntries(), is(100L));
		assertThat(config.getDiskSizeInMB(), is(512L));
		assertThat(config.getExpiryInMinutes(), is(60L));
	}

	@SuppressWarnings("rawtypes")
	@Test
	void sessionCacheConfigWithOffHeapAndDiskConstructor() {
		SessionCacheConfig config = new SessionCacheConfig(50L, 256L, 1024L, 45L);
		assertThat(config.getOffHeapSizeInMB(), is(256L));
		assertThat(config.getDiskSizeInMB(), is(1024L));
		assertThat(config.getExpiryInMinutes(), is(45L));
	}

	@SuppressWarnings("rawtypes")
	@Test
	void sessionCacheConfigToStringNotNull() {
		SessionCacheConfig config = new SessionCacheConfig(100L, 30L);
		assertThat(config.toString(), notNullValue());
	}

	@SuppressWarnings("rawtypes")
	@Test
	void sessionCacheConfigNameIsAlwaysSessions() {
		SessionCacheConfig config = new SessionCacheConfig(100L, 30L);
		assertThat(config.getName(), is("sessions"));
	}
}
