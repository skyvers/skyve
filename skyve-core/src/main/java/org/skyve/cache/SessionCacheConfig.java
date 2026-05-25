package org.skyve.cache;

import java.util.TreeSet;

/**
 * Configures the web session index cache.
 *
 * <p>Entries are keyed by session identifier and retain a set of related
 * values. Expiry uses time-to-idle semantics.
 */
@SuppressWarnings("rawtypes")
public class SessionCacheConfig extends EHCacheConfig<String, TreeSet> {
	private static final String SESSIONS_CACHE_NAME = "sessions";

	/**
	 * Creates a heap-only session cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public SessionCacheConfig(long heapSizeEntries, long expiryInMinutes) {
		super(SESSIONS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class);
	}

	/**
	 * Creates a heap-plus-disk session cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public SessionCacheConfig(long heapSizeEntries,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(SESSIONS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class, diskSizeInMB);
	}

	/**
	 * Creates a heap/off-heap/disk session cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public SessionCacheConfig(long heapSizeEntries,
									long offHeapSizeInMB,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(SESSIONS_CACHE_NAME, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, TreeSet.class, diskSizeInMB);
	}
	
	/**
	 * Returns a diagnostic representation of this configuration.
	 *
	 * @return config details in JSON-like form
	 */
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
