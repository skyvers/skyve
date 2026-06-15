package org.skyve.cache;

/**
 * Configures the conversation snapshot cache.
 *
 * <p>Entries are keyed by conversation identifier and expire using
 * time-to-idle semantics.
 */
public class ConversationCacheConfig extends EHCacheConfig<String, byte[]> {
	private static final String CONVERSATIONS_CACHE_NAME = "conversations";

	/**
	 * Creates a heap-only conversation cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public ConversationCacheConfig(long heapSizeEntries, long expiryInMinutes) {
		super(CONVERSATIONS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, byte[].class);
	}

	/**
	 * Creates a heap-plus-disk conversation cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public ConversationCacheConfig(long heapSizeEntries,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(CONVERSATIONS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, byte[].class, diskSizeInMB);
	}

	/**
	 * Creates a heap/off-heap/disk conversation cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public ConversationCacheConfig(long heapSizeEntries,
									long offHeapSizeInMB,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(CONVERSATIONS_CACHE_NAME, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, byte[].class, diskSizeInMB);
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
