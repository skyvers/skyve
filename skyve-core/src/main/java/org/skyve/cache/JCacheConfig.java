package org.skyve.cache;

import java.io.Serializable;

/**
 * Configures a JCache-backed cache region.
 *
 * <p>This descriptor defines sizing and expiry policy values consumed by
 * JCache provider bootstrap.
 *
 * @param <K> cache key type
 * @param <V> cache value type
 */
public class JCacheConfig<K extends Serializable, V extends Serializable> extends CacheConfig<K, V> {
	/**
	 * Creates a heap-only config with explicit expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 * @param keyClass key type
	 * @param valueClass value type
	 */
	public JCacheConfig(String name,
							long heapSizeEntries,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}

	/**
	 * Creates a heap-only config with provider defaults for expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param keyClass key type
	 * @param valueClass value type
	 */
	public JCacheConfig(String name, long heapSizeEntries, Class<K> keyClass, Class<V> valueClass) {
		super(name, heapSizeEntries, keyClass, valueClass);
	}

	/**
	 * Creates a heap/off-heap config with explicit expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 * @param keyClass key type
	 * @param valueClass value type
	 */
	public JCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}

	/**
	 * Creates a heap/off-heap config with provider defaults for expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param keyClass key type
	 * @param valueClass value type
	 */
	public JCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, offHeapSizeInMB, keyClass, valueClass);
	}
	
	/**
	 * Returns a diagnostic representation of this configuration.
	 *
	 * @return config details in JSON-like form
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(128);
		sb.append("{name:\"").append(getName());
		sb.append("\",type:\"jcache");
		sb.append("\",heapSizeEntries:").append(getHeapSizeEntries());
		sb.append(",offHeapSizeInMB:").append(getOffHeapSizeInMB());
		sb.append(",expiryPolicy:\"").append(getExpiryPolicy());
		sb.append("\",expiryInMinutes:").append(getExpiryInMinutes());
		sb.append(",keyClass:\"").append(getKeyClass());
		sb.append("\",valueClass:\"").append(getValueClass()).append("\"}");
		return sb.toString();
	}
}
