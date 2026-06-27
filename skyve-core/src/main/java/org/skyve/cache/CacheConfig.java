package org.skyve.cache;

import java.io.Serializable;

/**
 * Abstract cache configuration that declares the sizing and expiry policy for a named cache.
 *
 * <p>Concrete subclasses ({@link EHCacheConfig}, {@link JCacheConfig}) carry type-safe
 * configuration for EHCache and JCache instances respectively. The configuration is
 * consumed by {@link org.skyve.cache.Caching#createEHCache}/{@link org.skyve.cache.Caching#createJCache}
 * at application startup.
 *
 * @param <K> the serialisable key type
 * @param <V> the serialisable value type
 * @see EHCacheConfig
 * @see JCacheConfig
 * @see CacheExpiryPolicy
 */
public abstract class CacheConfig<K extends Serializable, V extends Serializable> {
	private String name;
	private long heapSizeEntries;
	private long offHeapSizeInMB;
	private long expiryInMinutes;
	private CacheExpiryPolicy expiryPolicy;
	private Class<K> keyClass;
	private Class<V> valueClass;
	
	/**
	 * Creates a new CacheConfig instance.
	 * @param name the name
	 * @param heapSizeEntries the heapSizeEntries
	 * @param offHeapSizeInMB the offHeapSizeInMB
	 * @param expiryPolicy the expiryPolicy
	 * @param expiryInMinutes the expiryInMinutes
	 * @param keyClass the keyClass
	 * @param valueClass the valueClass
	 */
	protected CacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		this.name = name;
		this.heapSizeEntries = heapSizeEntries;
		this.offHeapSizeInMB = offHeapSizeInMB;
		this.expiryPolicy = expiryPolicy;
		this.expiryInMinutes = expiryInMinutes;
		this.keyClass = keyClass;
		this.valueClass = valueClass;
	}

	/**
	 * Creates a new CacheConfig instance.
	 * @param name the name
	 * @param heapSizeEntries the heapSizeEntries
	 * @param expiryPolicy the expiryPolicy
	 * @param expiryInMinutes the expiryInMinutes
	 * @param keyClass the keyClass
	 * @param valueClass the valueClass
	 */
	protected CacheConfig(String name,
							long heapSizeEntries,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		this(name, heapSizeEntries, 0, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}
	
 	/**
 	 * Creates a new CacheConfig instance.
 	 * @param name the name
 	 * @param heapSizeEntries the heapSizeEntries
 	 * @param offHeapSizeInMB the offHeapSizeInMB
 	 * @param keyClass the keyClass
 	 * @param valueClass the valueClass
 	 */
 	protected CacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							Class<K> keyClass,
							Class<V> valueClass) {
		this(name, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.eternal, 0, keyClass, valueClass);
	}
 	
 	/**
 	 * Creates a new CacheConfig instance.
 	 * @param name the name
 	 * @param heapSizeEntries the heapSizeEntries
 	 * @param keyClass the keyClass
 	 * @param valueClass the valueClass
 	 */
 	protected CacheConfig(String name,
							long heapSizeEntries,
							Class<K> keyClass,
							Class<V> valueClass) {
		this(name, heapSizeEntries, 0, keyClass, valueClass);
 	}

	/**
	 * Returns the name.
	 * @return the result
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns the heapSizeEntries.
	 * @return the result
	 */
	public long getHeapSizeEntries() {
		return heapSizeEntries;
	}

	/**
	 * Returns the offHeapSizeInMB.
	 * @return the result
	 */
	public long getOffHeapSizeInMB() {
		return offHeapSizeInMB;
	}

	/**
	 * Returns the expiryPolicy.
	 * @return the result
	 */
	public CacheExpiryPolicy getExpiryPolicy() {
		return expiryPolicy;
	}

	/**
	 * Returns the expiryInMinutes.
	 * @return the result
	 */
	public long getExpiryInMinutes() {
		return expiryInMinutes;
	}

	/**
	 * Returns the keyClass.
	 * @return the result
	 */
	public Class<K> getKeyClass() {
		return keyClass;
	}

	/**
	 * Returns the valueClass.
	 * @return the result
	 */
	public Class<V> getValueClass() {
		return valueClass;
	}
}
