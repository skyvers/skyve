package org.skyve.cache;

import java.io.Serializable;

/**
 * Configures second-level Hibernate cache regions.
 *
 * <p>Uses {@link Serializable} keys and values so entity, collection, and
 * query cache payloads can share one descriptor type.
 */
public class HibernateCacheConfig extends JCacheConfig<Serializable, Serializable> {

	/**
	 * Creates a heap-only Hibernate region config with explicit expiry.
	 *
	 * @param name cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 */
	public HibernateCacheConfig(String name,
									long heapSizeEntries,
									CacheExpiryPolicy expiryPolicy,
									long expiryInMinutes) {
		super(name, heapSizeEntries, expiryPolicy, expiryInMinutes, Serializable.class, Serializable.class);
	}

	/**
	 * Creates a heap/off-heap Hibernate region config with explicit expiry.
	 *
	 * @param name cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param expiryPolicy expiry policy to apply
	 * @param expiryInMinutes expiry duration in minutes
	 */
	public HibernateCacheConfig(String name,
									long heapSizeEntries,
									long offHeapSizeInMB,
									CacheExpiryPolicy expiryPolicy,
									long expiryInMinutes) {
		super(name, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, Serializable.class, Serializable.class);
	}

	/**
	 * Creates a heap-only Hibernate region config with provider defaults for expiry.
	 *
	 * @param name cache region name
	 * @param heapSizeEntries maximum heap entry count
	 */
	public HibernateCacheConfig(String name, long heapSizeEntries) {
		super(name, heapSizeEntries, Serializable.class, Serializable.class);
	}

	/**
	 * Creates a heap/off-heap Hibernate region config with provider defaults for expiry.
	 *
	 * @param name cache region name
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 */
	public HibernateCacheConfig(String name, long heapSizeEntries, long offHeapSizeInMB) {
		super(name, heapSizeEntries, offHeapSizeInMB, Serializable.class, Serializable.class);
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
		sb.append("\",heapSizeEntries:").append(getHeapSizeEntries());
		sb.append(",offHeapSizeInMB:").append(getOffHeapSizeInMB());
		sb.append(",expiryPolicy:\"").append(getExpiryPolicy());
		sb.append("\",expiryInMinutes:").append(getExpiryInMinutes()).append('}');
		return sb.toString();
	}
}
