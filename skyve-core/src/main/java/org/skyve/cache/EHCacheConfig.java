package org.skyve.cache;

import java.io.Serializable;

/**
 * Configures an Ehcache-backed cache region.
 *
 * <p>Extends {@link CacheConfig} with optional disk tier size and persistence
 * settings used by Ehcache providers.
 *
 * <p>Instances are mutable configuration descriptors and should be fully
 * initialized before publication to cache bootstrap code.
 *
 * @param <K> cache key type
 * @param <V> cache value type
 */
public class EHCacheConfig<K extends Serializable, V extends Serializable> extends CacheConfig<K, V> {
	private long diskSizeInMB = 0;
	private boolean persistent = false;

	/**
	 * Creates a config with heap/off-heap/disk tiers and explicit expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB) {
		super(name, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, keyClass, valueClass);
		this.diskSizeInMB = diskSizeInMB;
	}

	/**
	 * Creates a config with heap/off-heap/disk tiers, expiry, and persistence.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param persistent whether disk storage is persistent across restarts
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB,
							boolean persistent) {
		this(name, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, keyClass, valueClass, diskSizeInMB);
		this.persistent = persistent;
	}


	/**
	 * Creates a config with heap and off-heap tiers plus explicit expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 * @param keyClass key type
	 * @param valueClass value type
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}

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
	public EHCacheConfig(String name,
							long heapSizeEntries,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}

	/**
	 * Creates a heap-plus-disk config with explicit expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB) {
		super(name, heapSizeEntries, expiryPolicy, expiryInMinutes, keyClass, valueClass);
		this.diskSizeInMB = diskSizeInMB;
	}

	/**
	 * Creates a heap-plus-disk config with explicit expiry and persistence.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param persistent whether disk storage is persistent across restarts
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB,
							boolean persistent) {
		this(name, heapSizeEntries, expiryPolicy, expiryInMinutes, keyClass, valueClass, diskSizeInMB);
		this.persistent = persistent;
	}

	/**
	 * Creates a heap-only config with provider defaults for expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param keyClass key type
	 * @param valueClass value type
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, keyClass, valueClass);
	}

	/**
	 * Creates a heap-plus-disk config with provider defaults for expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB) {
		super(name, heapSizeEntries, keyClass, valueClass);
		this.diskSizeInMB = diskSizeInMB;
	}

	/**
	 * Creates a heap-plus-disk config with persistence and provider defaults for expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param persistent whether disk storage is persistent across restarts
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB,
							boolean persistent) {
		this(name, heapSizeEntries, keyClass, valueClass, diskSizeInMB);
		this.persistent = persistent;
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
	public EHCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, offHeapSizeInMB, keyClass, valueClass);
	}

	/**
	 * Creates a heap/off-heap/disk config with provider defaults for expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB) {
		super(name, heapSizeEntries, offHeapSizeInMB, keyClass, valueClass);
		this.diskSizeInMB = diskSizeInMB;
	}

	/**
	 * Creates a heap/off-heap/disk config with persistence and provider defaults for expiry.
	 *
	 * @param name the cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param keyClass key type
	 * @param valueClass value type
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param persistent whether disk storage is persistent across restarts
	 */
	public EHCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							Class<K> keyClass,
							Class<V> valueClass,
							long diskSizeInMB,
							boolean persistent) {
		this(name, heapSizeEntries, offHeapSizeInMB, keyClass, valueClass, diskSizeInMB);
		this.persistent = persistent;
	}

	/**
	 * Returns the configured disk tier size.
	 *
	 * @return disk size in megabytes
	 */
	public long getDiskSizeInMB() {
		return diskSizeInMB;
	}

	/**
	 * Indicates whether disk persistence is enabled.
	 *
	 * @return {@code true} when disk data survives cache manager restarts
	 */
	public boolean isPersistent() {
		return persistent;
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
		sb.append("\",type:\"ehcache");
		sb.append("\",heapSizeEntries:").append(getHeapSizeEntries());
		sb.append(",offHeapSizeInMB:").append(getOffHeapSizeInMB());
		sb.append(",diskSizeInMB:").append(getDiskSizeInMB());
		sb.append(",persistent:").append(isPersistent());
		sb.append(",expiryPolicy:\"").append(getExpiryPolicy());
		sb.append("\",expiryInMinutes:").append(getExpiryInMinutes());
		sb.append(",keyClass:\"").append(getKeyClass());
		sb.append("\",valueClass:\"").append(getValueClass()).append("\"}");
		return sb.toString();
	}
}
