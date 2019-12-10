package org.skyve.cache;

import java.io.Serializable;

public abstract class CacheConfig<K extends Serializable, V extends Serializable> {
	private String name;
	private long heapSizeEntries;
	private long offHeapSizeInMB;
	private long expiryInMinutes;
	private CacheExpiryPolicy expiryPolicy;
	private Class<K> keyClass;
	private Class<V> valueClass;
	
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

	protected CacheConfig(String name,
							long heapSizeEntries,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		this(name, heapSizeEntries, 0, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}
	
 	protected CacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							Class<K> keyClass,
							Class<V> valueClass) {
		this(name, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.eternal, 0, keyClass, valueClass);
	}
 	
 	protected CacheConfig(String name,
							long heapSizeEntries,
							Class<K> keyClass,
							Class<V> valueClass) {
		this(name, heapSizeEntries, 0, keyClass, valueClass);
 	}

	public String getName() {
		return name;
	}

	public long getHeapSizeEntries() {
		return heapSizeEntries;
	}

	public long getOffHeapSizeInMB() {
		return offHeapSizeInMB;
	}

	public CacheExpiryPolicy getExpiryPolicy() {
		return expiryPolicy;
	}

	public long getExpiryInMinutes() {
		return expiryInMinutes;
	}

	public Class<K> getKeyClass() {
		return keyClass;
	}

	public Class<V> getValueClass() {
		return valueClass;
	}
}
