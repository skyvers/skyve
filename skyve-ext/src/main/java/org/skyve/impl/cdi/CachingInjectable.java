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
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class CachingInjectable implements Caching, Serializable {
	private static final long serialVersionUID = -1805372150480306143L;

	@Override
	public void startup() {
		EXT.getCaching().startup();
	}

	@Override
	public void shutdown() {
		EXT.getCaching().shutdown();
	}

	@Override
	public <K, V> Cache<K, V> getEHCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return EXT.getCaching().getEHCache(name, keyClass, valueClass);
	}

	@Override
	public <K, V> javax.cache.Cache<K, V> getJCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return EXT.getCaching().getJCache(name, keyClass, valueClass);
	}

	@Override
	public <K extends Serializable, V extends Serializable> Cache<K, V> createEHCache(EHCacheConfig<K, V> config) {
		return EXT.getCaching().createEHCache(config);
	}

	@Override
	public <K extends Serializable, V extends Serializable> javax.cache.Cache<K, V> createJCache(JCacheConfig<K, V> config) {
		return EXT.getCaching().createJCache(config);
	}

	@Override
	public void removeEHCache(String name) {
		EXT.getCaching().removeEHCache(name);
	}

	@Override
	public void destroyEHCache(String name) throws CachePersistenceException {
		EXT.getCaching().destroyEHCache(name);
	}

	@Override
	public void destroyJCache(String name) {
		EXT.getCaching().destroyJCache(name);
	}

	@Override
	public CacheStatistics getEHCacheStatistics(String name) {
		return EXT.getCaching().getEHCacheStatistics(name);
	}

	@Override
	public TierStatistics getEHTierStatistics(CacheStatistics statistics, CacheTier tier) {
		return EXT.getCaching().getEHTierStatistics(statistics, tier);
	}

	@Override
	public CacheStatisticsMXBean getJCacheStatisticsMXBean(String name) {
		return EXT.getCaching().getJCacheStatisticsMXBean(name);
	}

	@Override
	public PersistentCacheManager getEHCacheManager() {
		return EXT.getCaching().getEHCacheManager();
	}

	@Override
	public CacheManager getJCacheManager() {
		return EXT.getCaching().getJCacheManager();
	}
}
