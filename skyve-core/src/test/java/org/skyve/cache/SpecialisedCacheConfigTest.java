package org.skyve.cache;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class SpecialisedCacheConfigTest {

	@Test
	@SuppressWarnings("static-method")
	public void geoIPCacheConfigHeapOnly() {
		GeoIPCacheConfig cfg = new GeoIPCacheConfig(100L, 30L);
		assertEquals(100L, cfg.getHeapSizeEntries());
		assertEquals(30L, cfg.getExpiryInMinutes());
	}

	@Test
	@SuppressWarnings("static-method")
	public void geoIPCacheConfigWithDisk() {
		GeoIPCacheConfig cfg = new GeoIPCacheConfig(100L, 64L, 30L);
		assertEquals(64L, cfg.getDiskSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	public void geoIPCacheConfigToStringContainsHeapSize() {
		GeoIPCacheConfig cfg = new GeoIPCacheConfig(50L, 15L);
		assertThat(cfg.toString(), containsString("heapSizeEntries:50"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void csrfTokenCacheConfigHeapOnly() {
		CSRFTokenCacheConfig cfg = new CSRFTokenCacheConfig(200L, 60L);
		assertEquals(200L, cfg.getHeapSizeEntries());
		assertEquals(60L, cfg.getExpiryInMinutes());
	}

	@Test
	@SuppressWarnings("static-method")
	public void csrfTokenCacheConfigWithDisk() {
		CSRFTokenCacheConfig cfg = new CSRFTokenCacheConfig(200L, 32L, 60L);
		assertEquals(32L, cfg.getDiskSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	public void csrfTokenCacheConfigToString() {
		CSRFTokenCacheConfig cfg = new CSRFTokenCacheConfig(10L, 5L);
		assertThat(cfg.toString(), containsString("expiryInMinutes:5"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void conversationCacheConfigHeapOnly() {
		ConversationCacheConfig cfg = new ConversationCacheConfig(500L, 120L);
		assertEquals(500L, cfg.getHeapSizeEntries());
		assertEquals(120L, cfg.getExpiryInMinutes());
	}

	@Test
	@SuppressWarnings("static-method")
	public void conversationCacheConfigWithDisk() {
		ConversationCacheConfig cfg = new ConversationCacheConfig(500L, 256L, 120L);
		assertEquals(256L, cfg.getDiskSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	public void conversationCacheConfigWithOffHeap() {
		ConversationCacheConfig cfg = new ConversationCacheConfig(500L, 128L, 256L, 120L);
		assertEquals(128L, cfg.getOffHeapSizeInMB());
		assertEquals(256L, cfg.getDiskSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	public void conversationCacheConfigToString() {
		ConversationCacheConfig cfg = new ConversationCacheConfig(20L, 10L);
		assertThat(cfg.toString(), containsString("diskSizeInMB:0"));
	}

	// --- HibernateCacheConfig ---

	@Test
	@SuppressWarnings("static-method")
	public void hibernateCacheConfigMinimalConstructor() {
		HibernateCacheConfig cfg = new HibernateCacheConfig("myCache", 100L);
		assertEquals(100L, cfg.getHeapSizeEntries());
		assertThat(cfg.toString(), containsString("myCache"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void hibernateCacheConfigWithOffHeap() {
		HibernateCacheConfig cfg = new HibernateCacheConfig("myCache", 100L, 64L);
		assertEquals(64L, cfg.getOffHeapSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	public void hibernateCacheConfigWithExpiryPolicy() {
		HibernateCacheConfig cfg = new HibernateCacheConfig("myCache", 100L, CacheExpiryPolicy.timeToLive, 30L);
		assertEquals(30L, cfg.getExpiryInMinutes());
	}

	@Test
	@SuppressWarnings("static-method")
	public void hibernateCacheConfigWithOffHeapAndExpiryPolicy() {
		HibernateCacheConfig cfg = new HibernateCacheConfig("myCache", 100L, 32L, CacheExpiryPolicy.timeToIdle, 60L);
		assertEquals(32L, cfg.getOffHeapSizeInMB());
		assertEquals(60L, cfg.getExpiryInMinutes());
	}
}
