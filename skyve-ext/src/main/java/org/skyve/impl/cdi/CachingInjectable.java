package org.skyve.impl.cdi;

import java.io.Serializable;

import javax.cache.CacheManager;
import javax.cache.management.CacheStatisticsMXBean;

import org.ehcache.Cache;
import org.ehcache.CachePersistenceException;
import org.ehcache.PersistentCacheManager;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.skyve.EXT;
import org.skyve.cache.CacheTier;
import org.skyve.cache.Caching;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.JCacheConfig;

import jakarta.enterprise.inject.Alternative;

/**
 * Stateless CDI proxy for {@link Caching}.
 *
 * <p>Delegates cache operations to {@link EXT#getCaching()} to keep cache APIs
 * available after passivation without serializing cache manager state.
 */
@Alternative
public class CachingInjectable implements Caching, Serializable {
	private static final long serialVersionUID = -1805372150480306143L;

	/**
	 * Starts cache infrastructure.
	 */
	@Override
	public void startup() {
		EXT.getCaching().startup();
	}

	/**
	 * Stops cache infrastructure.
	 */
	@Override
	public void shutdown() {
		EXT.getCaching().shutdown();
	}

	/**
	 * Returns an EHCache instance by cache name and key/value types.
	 *
	 * @param name the cache region name.
	 * @param keyClass the key type for the cache.
	 * @param valueClass the value type for the cache.
	 * @param <K> the cache key type.
	 * @param <V> the cache value type.
	 * @return the resolved EHCache instance.
	 */

	@Override
	public <K, V> Cache<K, V> getEHCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return EXT.getCaching().getEHCache(name, keyClass, valueClass);
	}

	/**
	 * Returns a JCache instance by cache name and key/value types.
	 *
	 * @param name the cache region name.
	 * @param keyClass the key type for the cache.
	 * @param valueClass the value type for the cache.
	 * @param <K> the cache key type.
	 * @param <V> the cache value type.
	 * @return the resolved JCache instance.
	 */

	@Override
	@SuppressWarnings("resource") // Cache lifecycle is owned by the shared EXT caching service.
	public <K, V> javax.cache.Cache<K, V> getJCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return EXT.getCaching().getJCache(name, keyClass, valueClass);
	}

	/**
	 * Creates or returns an EHCache using the supplied cache configuration.
	 *
	 * @param config the EHCache configuration descriptor.
	 * @param <K> the cache key type.
	 * @param <V> the cache value type.
	 * @return the created or existing EHCache instance.
	 */

	@Override
	public <K extends Serializable, V extends Serializable> Cache<K, V> createEHCache(EHCacheConfig<K, V> config) {
		return EXT.getCaching().createEHCache(config);
	}

	/**
	 * Creates or returns a JCache using the supplied cache configuration.
	 *
	 * @param config the JCache configuration descriptor.
	 * @param <K> the cache key type.
	 * @param <V> the cache value type.
	 * @return the created or existing JCache instance.
	 */

	@Override
	@SuppressWarnings("resource") // Cache lifecycle is owned by the shared EXT caching service.
	public <K extends Serializable, V extends Serializable> javax.cache.Cache<K, V> createJCache(JCacheConfig<K, V> config) {
		return EXT.getCaching().createJCache(config);
	}

	/**
	 * Removes an EHCache cache region from the active manager.
	 *
	 * @param name the cache region name.
	 */
	@Override
	public void removeEHCache(String name) {
		EXT.getCaching().removeEHCache(name);
	}

	/**
	 * Destroys a persistent EHCache region and its on-disk state.
	 *
	 * @param name the cache region name.
	 * @throws CachePersistenceException if persistence cleanup fails.
	 */
	@Override
	public void destroyEHCache(String name) throws CachePersistenceException {
		EXT.getCaching().destroyEHCache(name);
	}

	/**
	 * Destroys a JCache region by name.
	 *
	 * @param name the cache region name.
	 */
	@Override
	public void destroyJCache(String name) {
		EXT.getCaching().destroyJCache(name);
	}

	/**
	 * Returns EHCache runtime statistics for the named cache.
	 *
	 * @param name the cache region name.
	 * @return runtime statistics for the named cache.
	 */
	@Override
	public CacheStatistics getEHCacheStatistics(String name) {
		return EXT.getCaching().getEHCacheStatistics(name);
	}

	/**
	 * Returns tier-level statistics (heap/off-heap/disk) for a cache.
	 *
	 * @param statistics cache-level statistics source.
	 * @param tier the tier to inspect.
	 * @return statistics for the requested tier.
	 */
	@Override
	public TierStatistics getEHTierStatistics(CacheStatistics statistics, CacheTier tier) {
		return EXT.getCaching().getEHTierStatistics(statistics, tier);
	}

	/**
	 * Returns JCache statistics MBean for the named cache.
	 *
	 * @param name the cache region name.
	 * @return the statistics MBean for the named JCache.
	 */
	@Override
	public CacheStatisticsMXBean getJCacheStatisticsMXBean(String name) {
		return EXT.getCaching().getJCacheStatisticsMXBean(name);
	}

	/**
	 * Returns the underlying EHCache manager.
	 *
	 * @return the active EHCache manager.
	 */

	@Override
	@SuppressWarnings("resource") // Cache manager lifecycle is owned by the shared EXT caching service.
	public PersistentCacheManager getEHCacheManager() {
		return EXT.getCaching().getEHCacheManager();
	}

	/**
	 * Returns the underlying JCache manager.
	 *
	 * @return the active JCache manager.
	 */

	@Override
	@SuppressWarnings("resource") // Cache manager lifecycle is owned by the shared EXT caching service.
	public CacheManager getJCacheManager() {
		return EXT.getCaching().getJCacheManager();
	}
}
