package org.skyve.metadata.repository;

import org.skyve.metadata.customer.Customer;

import jakarta.annotation.Nullable;

public interface CachedRepository extends Repository {
	/**
	 * 
	 * @param customer if <code>null</code>, the entire repository goes.
	 */
	void evictCachedMetaData(@Nullable Customer customer);
}
