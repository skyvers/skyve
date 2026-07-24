package org.skyve.cache;

import java.io.Serializable;

import javax.cache.management.CacheStatisticsMXBean;

import org.ehcache.Cache;
import org.ehcache.CachePersistenceException;
import org.ehcache.PersistentCacheManager;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.skyve.impl.util.SystemObserver;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Allows obtaining defined caches and management of EHCaches or JCaches.
 * @author mike
 */
public interface Caching extends SystemObserver {
	/**
	 * Determine whether an EHCache exists by name.
	 * @param name The cache name.
	 * @return {@code true} when the cache exists.
	 */
	boolean isEHCache(@Nonnull String name);

	/**
	 * Get an existing EHCache by name.
	 * @return The cache.
	 */
	@Nonnull <K extends Object, V extends Object> Cache<K, V> getEHCache(@Nonnull String name,
																			@Nonnull Class<K> keyClass,
																			@Nonnull Class<V> valueClass);

	/**
	 * Determine whether a JCache exists by name.
	 * @param name The cache name.
	 * @return {@code true} when the cache exists.
	 */
	boolean isJCache(@Nonnull String name);

	/**
	 * Get an existing JCache by name.
	 * @return The cache.
	 */
	@Nonnull <K extends Object, V extends Object> javax.cache.Cache<K, V> getJCache(@Nonnull String name,
																						@Nonnull Class<K> keyClass,
																						@Nonnull Class<V> valueClass);

	/**
	 * Create a new EHCache with the given parameters
	 * @param <K>	Key type
	 * @param <V>	Value Type
	 * @param config	A cache configuration
	 * @return	An EHCache configured ready for use.
	 */
	@Nonnull <K extends Serializable, V extends Serializable> Cache<K, V> createEHCache(@Nonnull EHCacheConfig<K, V> config);

	/**
	 * Create a new JCache with the given parameters
	 * @param <K>	Key type
	 * @param <V>	Value Type
	 * @param config	A cache configuration.
	 * @return	A JCache configured ready for use.
	 */
	@Nonnull <K extends Serializable, V extends Serializable> javax.cache.Cache<K, V> createJCache(@Nonnull JCacheConfig<K, V> config);

	/**
	 * Remove an existing EHCache by name
	 */
	void removeEHCache(@Nonnull String name);
	
	/**
	 * Destroy an existing persisted EHCache by name
	 */
	void destroyEHCache(@Nonnull String name) throws CachePersistenceException;

	/**
	 * Destroy an existing JCache by name
	 */
	void destroyJCache(@Nonnull String name);

	/**
	 * Get the statistics for a named EHCache.
	 * @param name	The cache name
	 * @return	The statistics.
	 */
	@Nonnull CacheStatistics getEHCacheStatistics(@Nonnull String name);
	
	/**
	 * Get the tier statistics from the EHCache statistics
	 * @param statistics	The cache statistics.
	 * @param tier	The tier to find statistics for.
	 * @return	The statistics for the tier or null if the tier is not in use.
	 */
	@Nullable TierStatistics getEHTierStatistics(@Nonnull CacheStatistics statistics, @Nonnull CacheTier tier);
	
	/**
	 * Get the statistics MBean for a JCache
	 * @param name	The cache name.
	 * @return	The statistics, or {@code null} if statistics MBean do not exist.
	 */
	@Nullable CacheStatisticsMXBean getJCacheStatisticsMXBean(@Nonnull String name);
	
	/**
	 * Get EHCacheManager.
	 * @return	EHCacheManager.
	 */
	@Nonnull PersistentCacheManager getEHCacheManager();

	/**
	 * Get JCacheManager.
	 * @return	JCacheManager.
	 */
	@Nonnull javax.cache.CacheManager getJCacheManager();
}
