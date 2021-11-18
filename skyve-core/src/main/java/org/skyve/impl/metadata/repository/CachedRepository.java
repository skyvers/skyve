package org.skyve.impl.metadata.repository;

import java.util.concurrent.ConcurrentHashMap;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;

public abstract class CachedRepository extends ProvidedRepositoryFactory {
	/**
	 * The cache.
	 * MetaData namespace and name -> MetaData.
	 * Thread-safe and performant for mostly-read operations.
	 */
	private ConcurrentHashMap<String, MetaData> cache = new ConcurrentHashMap<>();

	@SuppressWarnings("unchecked")
	protected <T extends MetaData> T get(String name) {
		return (T) cache.get(name);
	}

	/**
	 * Cache a piece of named metadata.
	 * 
	 * @param name The name of the metadata
	 * @param metaData The metadata.
	 */
	protected void put(String name, MetaData metaData) {
		MetaData oldMetaData = cache.put(name, metaData);
		if (oldMetaData != null) {
			throw new MetaDataException("NAME CLASH - " + name + " is already used for " + oldMetaData);
		}
	}

	@Override
	public void evictCachedMetaData(Customer customer) {
		if (customer == null) {
			cache.clear();
		}
	}
}
