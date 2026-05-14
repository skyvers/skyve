package org.skyve.cache;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class JCacheConfigTest {

	@Test
	@SuppressWarnings("static-method")
	public void heapOnlyConstructor() {
		JCacheConfig<String, String> cfg = new JCacheConfig<>("myCache", 500L, String.class, String.class);
		assertThat(cfg.getName(), containsString("myCache"));
		assertEquals(500L, cfg.getHeapSizeEntries());
	}

	@Test
	@SuppressWarnings("static-method")
	public void heapWithExpiryConstructor() {
		JCacheConfig<String, String> cfg = new JCacheConfig<>("expCache", 200L,
				CacheExpiryPolicy.timeToIdle, 60L, String.class, String.class);
		assertEquals(60L, cfg.getExpiryInMinutes());
	}

	@Test
	@SuppressWarnings("static-method")
	public void heapAndOffHeapConstructor() {
		JCacheConfig<String, String> cfg = new JCacheConfig<>("offHeapCache", 100L, 256L,
				String.class, String.class);
		assertEquals(256L, cfg.getOffHeapSizeInMB());
	}

	@Test
	@SuppressWarnings("static-method")
	public void heapOffHeapAndExpiryConstructor() {
		JCacheConfig<String, String> cfg = new JCacheConfig<>("fullCache", 100L, 128L,
				CacheExpiryPolicy.timeToLive, 30L, String.class, String.class);
		assertEquals(100L, cfg.getHeapSizeEntries());
		assertEquals(128L, cfg.getOffHeapSizeInMB());
		assertEquals(30L, cfg.getExpiryInMinutes());
	}

	@Test
	@SuppressWarnings("static-method")
	public void toStringContainsJcacheType() {
		JCacheConfig<String, String> cfg = new JCacheConfig<>("testCache", 10L, String.class, String.class);
		assertThat(cfg.toString(), containsString("jcache"));
		assertThat(cfg.toString(), containsString("testCache"));
	}
}
