package org.skyve.metadata.repository;

import org.skyve.metadata.customer.Customer;

import jakarta.annotation.Nullable;

/**
 * Extension of {@link Repository} that adds a cache-eviction operation.
 *
 * <p>Implementations cache loaded metadata (customers, modules, documents, views) for
 * performance. When metadata changes at runtime (e.g. after a hot-deploy or admin
 * override), the cache must be evicted to force a reload on next access.
 *
 * @see Repository
 * @see ProvidedRepository
 */
public interface CachedRepository extends Repository {
	/**
	 * Evicts all cached metadata for the given customer, forcing a reload on next access.
	 *
	 * <p>If {@code customer} is {@code null}, all cached metadata for every customer
	 * is discarded (full cache flush).
	 *
	 * @param customer  the customer whose metadata cache should be evicted,
	 *                  or {@code null} to flush the entire repository cache
	 */
	void evictCachedMetaData(@Nullable Customer customer);
}
