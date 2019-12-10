package org.skyve.cache;

import java.io.Serializable;

public class HibernateCacheConfig extends JCacheConfig<Serializable, Serializable> {

	public HibernateCacheConfig(String name,
									long heapSizeEntries,
									CacheExpiryPolicy expiryPolicy,
									long expiryInMinutes) {
		super(name, heapSizeEntries, expiryPolicy, expiryInMinutes, Serializable.class, Serializable.class);
	}

	public HibernateCacheConfig(String name,
									long heapSizeEntries,
									long offHeapSizeInMB,
									CacheExpiryPolicy expiryPolicy,
									long expiryInMinutes) {
		super(name, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, Serializable.class, Serializable.class);
	}

	public HibernateCacheConfig(String name, long heapSizeEntries) {
		super(name, heapSizeEntries, Serializable.class, Serializable.class);
	}

	public HibernateCacheConfig(String name, long heapSizeEntries, long offHeapSizeInMB) {
		super(name, heapSizeEntries, offHeapSizeInMB, Serializable.class, Serializable.class);
	}
	
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
