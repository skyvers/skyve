package org.skyve.cache;

import java.util.TreeSet;

@SuppressWarnings("rawtypes")
public class SessionCacheConfig extends EHCacheConfig<String, TreeSet> {
	private static final String SESSIONS_CACHE_NAME = "sessions";

	public SessionCacheConfig(long heapSizeEntries, long expiryInMinutes) {
		super(SESSIONS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class);
	}

	public SessionCacheConfig(long heapSizeEntries,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(SESSIONS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class, diskSizeInMB);
	}

	public SessionCacheConfig(long heapSizeEntries,
									long offHeapSizeInMB,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(SESSIONS_CACHE_NAME, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class, diskSizeInMB);
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
