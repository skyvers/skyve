package org.skyve.metadata.repository;

import javax.annotation.Nullable;

import org.skyve.metadata.customer.Customer;

public interface CachedRepository extends Repository {
	/**
	 * 
	 * @param customer if <code>null</code>, the entire repository goes.
	 */
	void evictCachedMetaData(@Nullable Customer customer);
}
