package org.skyve.cache;

import java.io.Serializable;

import javax.cache.management.CacheStatisticsMXBean;

import org.ehcache.Cache;
import org.ehcache.CachePersistenceException;
import org.ehcache.PersistentCacheManager;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.skyve.impl.util.SystemObserver;

/**
 * Allows obtaining defined caches and management of EHCaches or JCaches.
 * @author mike
 */
public interface Caching extends SystemObserver {
	/**
	 * Get an existing EHCache by name
	 */
	<K extends Object, V extends Object> Cache<K, V> getEHCache(String name, Class<K> keyClass, Class<V> valueClass);
	
	/**
	 * Get an existing JCache by name
	 */
	<K extends Object, V extends Object> javax.cache.Cache<K, V> getJCache(String name, Class<K> keyClass, Class<V> valueClass);

	/**
	 * Create a new EHCache with the given parameters
	 * @param <K>	Key type
	 * @param <V>	Value Type
	 * @param config	A cache configuration
	 * @return	An EHCache configured ready for use.
	 */
	<K extends Serializable, V extends Serializable> Cache<K, V> createEHCache(EHCacheConfig<K, V> config);

	/**
	 * Create a new JCache with the given parameters
	 * @param <K>	Key type
	 * @param <V>	Value Type
	 * @param config	A cache configuration.
	 * @return	A JCache configured ready for use.
	 */
	<K extends Serializable, V extends Serializable> javax.cache.Cache<K, V> createJCache(JCacheConfig<K, V> config);

	/**
	 * Remove an existing EHCache by name
	 */
	void removeEHCache(String name);
	
	/**
	 * Destroy an existing persisted EHCache by name
	 */
	void destroyEHCache(String name) throws CachePersistenceException;

	/**
	 * Destroy an existing JCache by name
	 */
	void destroyJCache(String name);

	/**
	 * Get the statistics for a named EHCache.
	 * @param name	The cache name
	 * @return	The statistics
	 */
	CacheStatistics getEHCacheStatistics(String name);
	
	/**
	 * Get the tier statistics from the EHCache statistics
	 * @param statistics	The cache statistics
	 * @param tier	The tier to find statistics for.
	 * @return	The statistics for the tier or null if the cache statistics is null or the tier is not in use.
	 */
	TierStatistics getEHTierStatistics(CacheStatistics statistics, CacheTier tier);
	
	/**
	 * Get the statistics MBean for a JCache
	 * @param name	The cache name.
	 * @return	The statistics.
	 */
	CacheStatisticsMXBean getJCacheStatisticsMXBean(String name);
	
	/**
	 * Get EHCacheManager.
	 * @return	EHCacheManager
	 */
	PersistentCacheManager getEHCacheManager();

	/**
	 * Get JCacheManager.
	 * @return	JCacheManager
	 */
	javax.cache.CacheManager getJCacheManager();
}
