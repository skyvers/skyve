package org.skyve.cache;

import java.util.TreeSet;

@SuppressWarnings("rawtypes")
public class CSRFTokenCacheConfig extends EHCacheConfig<String, TreeSet> {
	private static final String CSRF_TOKEN_CACHE_NAME = "tokens";

	public CSRFTokenCacheConfig(long heapSizeEntries, long expiryInMinutes) {
		super(CSRF_TOKEN_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class);
	}

	public CSRFTokenCacheConfig(long heapSizeEntries,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(CSRF_TOKEN_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class, diskSizeInMB);
	}

	public CSRFTokenCacheConfig(long heapSizeEntries,
									long offHeapSizeInMB,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(CSRF_TOKEN_CACHE_NAME, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class, diskSizeInMB);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(128);
		sb.append("{heapSizeEntries:").append(getHeapSizeEntries());
		sb.append(",offHeapSizeInMB:").append(getOffHeapSizeInMB());
		sb.append(",diskSizeInMB:").append(getDiskSizeInMB());
		sb.append(",expiryInMinutes:").append(getExpiryInMinutes()).append('}');
		return sb.toString();
	}
}
