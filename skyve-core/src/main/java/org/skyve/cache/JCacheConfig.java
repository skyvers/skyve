package org.skyve.cache;

import java.io.Serializable;

public class JCacheConfig<K extends Serializable, V extends Serializable> extends CacheConfig<K, V> {
	public JCacheConfig(String name,
							long heapSizeEntries,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}

	public JCacheConfig(String name, long heapSizeEntries, Class<K> keyClass, Class<V> valueClass) {
		super(name, heapSizeEntries, keyClass, valueClass);
	}

	public JCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							CacheExpiryPolicy expiryPolicy,
							long expiryInMinutes,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, keyClass, valueClass);
	}

	public JCacheConfig(String name,
							long heapSizeEntries,
							long offHeapSizeInMB,
							Class<K> keyClass,
							Class<V> valueClass) {
		super(name, heapSizeEntries, offHeapSizeInMB, keyClass, valueClass);
	}
	
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
