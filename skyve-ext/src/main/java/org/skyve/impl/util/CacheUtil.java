package org.skyve.impl.util;

import java.lang.management.ManagementFactory;
import java.time.Duration;
import java.util.Set;

import javax.cache.Caching;
import javax.cache.management.CacheStatisticsMXBean;
import javax.management.JMX;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import org.ehcache.Cache;
import org.ehcache.CacheManager;
import org.ehcache.Status;
import org.ehcache.config.CacheConfiguration;
import org.ehcache.config.builders.CacheConfigurationBuilder;
import org.ehcache.config.builders.CacheManagerBuilder;
import org.ehcache.config.builders.ExpiryPolicyBuilder;
import org.ehcache.config.builders.ResourcePoolsBuilder;
import org.ehcache.config.units.EntryUnit;
import org.ehcache.config.units.MemoryUnit;
import org.ehcache.core.spi.service.StatisticsService;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.DefaultStatisticsService;
import org.ehcache.core.statistics.TierStatistics;
import org.ehcache.jsr107.Eh107Configuration;

/**
 * Handles EHCaches and JCaches.
 * Creates and destroys EH and J caches at runtime.
 * Exposes the 2 CacheManagers and their various cache statistics.
 * 
 * Note:- disk persistence doesn't work when programmatically configured in JCache land and
 * 			a reduced set of statistics is availabel through the JCache MBean.
 * @author mike
 */
public class CacheUtil {
	private static CacheManager ehCacheManager;
	private static StatisticsService statisticsService = new DefaultStatisticsService();
	private static javax.cache.CacheManager jCacheManager;
	
	private CacheUtil() {
		// disallow instantiation
	}
	
	public static void init() {
		try {
			dispose(); // call this just in case the last deployment failed to get around ehcache's file lock.
		}
		finally {
			ehCacheManager = CacheManagerBuilder.newCacheManagerBuilder()
								.using(statisticsService)
								.with(CacheManagerBuilder.persistence(UtilImpl.CONTENT_DIRECTORY + "SKYVE_CACHE/"))
								.build(true);
			jCacheManager = Caching.getCachingProvider().getCacheManager();
		}
	}

	public static void dispose() {
		if ((ehCacheManager != null) && (! Status.UNINITIALIZED.equals(ehCacheManager.getStatus()))) {
			ehCacheManager.close();
		}
		if ((jCacheManager != null) && (! jCacheManager.isClosed())) {
			jCacheManager.close();
		}
	}

	/**
	 * Get EHCacheManager.
	 * @return	EHCacheManager
	 */
	public static CacheManager getEHCacheManager() {
		return ehCacheManager;
	}

	/**
	 * Get JCacheManager.
	 * @return	JCacheManager
	 */
	public static javax.cache.CacheManager getJCacheManager() {
		return jCacheManager;
	}

	/**
	 * Create a new EHCache with the given parameters in Skyve's cache manger
	 * @param <K>	Key type
	 * @param <V>	Value Type
	 * @param name	Cache name
	 * @param heapSize	Size of heap used for this cache in entries or MB (must be > 0)
	 * @param offHeapSizeInMB	Size of off heap memory used for this cache in MB (<= 0 means no off-heap).
	 * 							Do not forget to define in the java options the -XX:MaxDirectMemorySize option, according to the off-heap size you intend to use.
	 * @param diskSizeInMB	Size of disk storage used for this cache in MB (<= 0 means no disk)
	 * @param expiryInMinutes	Time to Idle expiry in minutes.
	 * @param keyClass	Key class
	 * @param valueClass	Value class
	 * @return	An EHCache configured ready for use.
	 */
	public static <K extends Object, V extends Object> Cache<K, V> createEHCache(String name,
																					long heapSizeEntries,
																					long offHeapSizeInMB,
																					long diskSizeInMB,
																					long expiryInMinutes,
																					Class<K> keyClass,
																					Class<V> valueClass) {
		ResourcePoolsBuilder rpb = ResourcePoolsBuilder.newResourcePoolsBuilder();
		rpb = rpb.heap(heapSizeEntries, EntryUnit.ENTRIES);
		if (offHeapSizeInMB > 0) {
			rpb = rpb.offheap(offHeapSizeInMB, MemoryUnit.MB);
		}
		if (diskSizeInMB > 0) {
			rpb = rpb.disk(diskSizeInMB, MemoryUnit.MB);
		}
		CacheConfigurationBuilder<K, V> ccb = CacheConfigurationBuilder.newCacheConfigurationBuilder(keyClass, valueClass, rpb);
		if (expiryInMinutes > 0) {
			ccb.withExpiry(ExpiryPolicyBuilder.timeToIdleExpiration(Duration.ofMinutes(expiryInMinutes)));
		}
		CacheConfiguration<K, V> cacheConfiguration = ccb.build();
		return ehCacheManager.createCache(name, cacheConfiguration); 
	}

	/**
	 * Create a new JCache with the given parameters in Skyve's cache manger
	 * @param <K>	Key type
	 * @param <V>	Value Type
	 * @param name	Cache name
	 * @param heapSize	Size of heap used for this cache in entries or MB (must be > 0)
	 * @param offHeapSizeInMB	Size of off heap memory used for this cache in MB (<= 0 means no off-heap).
	 * 							Do not forget to define in the java options the -XX:MaxDirectMemorySize option, according to the off-heap size you intend to use.
	 * @param expiryInMinutes	Time to Idle expiry in minutes.
	 * @param keyClass	Key class
	 * @param valueClass	Value class
	 * @return	A JCache configured ready for use.
	 */
	public static <K extends Object, V extends Object> javax.cache.Cache<K, V> createJCache(String name,
																								long heapSizeEntries,
																								long offHeapSizeInMB,
																								long expiryInMinutes,
																								Class<K> keyClass,
																								Class<V> valueClass) {
		ResourcePoolsBuilder rpb = ResourcePoolsBuilder.heap(heapSizeEntries);
		if (offHeapSizeInMB > 0) {
			rpb = rpb.offheap(offHeapSizeInMB, MemoryUnit.MB);
		}
		CacheConfigurationBuilder<K, V> configuration = 
				CacheConfigurationBuilder.newCacheConfigurationBuilder(keyClass, valueClass, rpb)
					.withExpiry(ExpiryPolicyBuilder.timeToIdleExpiration(Duration.ofMinutes(expiryInMinutes)));
		return getJCacheManager().createCache(name, Eh107Configuration.fromEhcacheCacheConfiguration(configuration));
	}

	/**
	 * Destroy an existing EHCache by name in Skyve's cache manger
	 */
	public static final void destroyEHCache(String name) {
		ehCacheManager.removeCache(name);
	}
	
	/**
	 * Destroy an existing JCache by name in Skyve's cache manger
	 */
	public static final void destroyJCache(String name) {
		jCacheManager.destroyCache(name);
	}

	/**
	 * Get an existing EHCache by name in Skyve's cache manger
	 */
	public static <K extends Object, V extends Object> Cache<K, V> getEHCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return ehCacheManager.getCache(name, keyClass, valueClass);
	}
	
	/**
	 * Get an existing JCache by name in Skyve's cache manger
	 */
	public static <K extends Object, V extends Object> javax.cache.Cache<K, V> getJCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return jCacheManager.getCache(name, keyClass, valueClass);
	}
	
	/**
	 * Get the statistics for a named EHCache.
	 * @param name	The cache name
	 * @return	The statistics
	 */
	public static CacheStatistics getEHCacheStatistics(String name) {
		return statisticsService.getCacheStatistics(name);
	}
	
	public static enum CacheTier {
		OnHeap, OffHeap, Disk
	}
	
	/**
	 * Get the tier statistics from the EHCache statistics
	 * @param statistics	The cache statistics
	 * @param tier	The tier to find statistics for.
	 * @return	The statistics for the tier or null if the cache statistics is null or the tier is not in use.
	 */
	public static TierStatistics getEHTierStatistics(CacheStatistics statistics, CacheTier tier) {
		return (statistics == null) ? null : statistics.getTierStatistics().get(tier.toString());
	}
	
	/**
	 * Get the statistics MBean for a JCache
	 * @param name	The cache name.
	 * @return	The statistics.
	 */
	public static CacheStatisticsMXBean getJCacheStatisticsMXBean(String name) {
		final MBeanServer mbeanServer = ManagementFactory.getPlatformMBeanServer();
		ObjectName objectName = null;
		try {
			objectName = new ObjectName("*:type=CacheStatistics,*,Cache=" + name);
		}
		catch (MalformedObjectNameException e) {
			UtilImpl.LOGGER.severe("Could not create statistics object name for cache " + name);
			e.printStackTrace();
		}
		Set<ObjectName> beans = mbeanServer.queryNames(objectName, null);
		if (beans.isEmpty()) {
			return null;
		}
		ObjectName[] objArray = beans.toArray(new ObjectName[beans.size()]);
		return JMX.newMBeanProxy(mbeanServer, objArray[0], CacheStatisticsMXBean.class);
	}
}
