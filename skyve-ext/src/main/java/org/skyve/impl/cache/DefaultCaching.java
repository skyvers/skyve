package org.skyve.impl.cache;

import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.time.Duration;
import java.util.Set;

import javax.cache.management.CacheStatisticsMXBean;
import javax.management.JMX;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import org.ehcache.Cache;
import org.ehcache.CachePersistenceException;
import org.ehcache.PersistentCacheManager;
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
import org.ehcache.core.internal.statistics.DefaultStatisticsService;
import org.ehcache.core.statistics.TierStatistics;
import org.ehcache.jsr107.Eh107Configuration;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.CacheExpiryPolicy;
import org.skyve.cache.Caching;
import org.skyve.cache.CacheTier;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

public class DefaultCaching implements Caching {
	private static final DefaultCaching INSTANCE = new DefaultCaching();

	private static PersistentCacheManager ehCacheManager;
	// This is a SPI class but there seems no clear way forward - see https://github.com/ehcache/ehcache3/issues/2951
	private static StatisticsService statisticsService = new DefaultStatisticsService();
	private static javax.cache.CacheManager jCacheManager;

	private DefaultCaching() {
		// nothing to see here
	}

	public static DefaultCaching get() {
		return INSTANCE;
	}

	@Override
	@SuppressWarnings("resource")
	public void startup() {
		if (isUnInitialised()) {
			try {
				shutdown(); // call this just in case the last deployment failed to get around ehcache's file lock.
			}
			finally {
				ehCacheManager = CacheManagerBuilder.newCacheManagerBuilder()
									.using(statisticsService)
									.with(CacheManagerBuilder.persistence(Util.getCacheDirectory()))
									.build(true);
				jCacheManager = javax.cache.Caching.getCachingProvider().getCacheManager();
				
				// Create the conversations cache
				if (UtilImpl.CONVERSATION_CACHE != null) {
					UtilImpl.LOGGER.info("Create the conversation cache with config " + UtilImpl.CONVERSATION_CACHE);
					createEHCache(UtilImpl.CONVERSATION_CACHE);
				}
	
				// Create the CSRF Token cache
				if (UtilImpl.CSRF_TOKEN_CACHE != null) {
					UtilImpl.LOGGER.info("Create the CSRF token cache with config " + UtilImpl.CSRF_TOKEN_CACHE);
					createEHCache(UtilImpl.CSRF_TOKEN_CACHE);
				}
	
				// Create the app caches
				for (CacheConfig<? extends Serializable, ? extends Serializable> config : UtilImpl.APP_CACHES) {
					UtilImpl.LOGGER.info("Create app cache with config " + config);
					createCache(config);
				}
				
				// Create the hibernate caches
				for (HibernateCacheConfig config : UtilImpl.HIBERNATE_CACHES) {
					UtilImpl.LOGGER.info("Create hibernate cache with config " + config);
					createJCache(config);
				}
			}
		}
	}

	@Override
	public void shutdown() {
		// NB all caches are closed by closing the cache managers
		if ((ehCacheManager != null) && (! Status.UNINITIALIZED.equals(ehCacheManager.getStatus()))) {
			ehCacheManager.close();
			ehCacheManager = null;
		}
		if ((jCacheManager != null) && (! jCacheManager.isClosed())) {
			jCacheManager.close();
			jCacheManager = null;
		}
	}

	@SuppressWarnings("resource")
	private <K extends Serializable, V extends Serializable> void createCache(CacheConfig<K, V> config) {
		if (config instanceof EHCacheConfig<?, ?>) {
			createEHCache((EHCacheConfig<K, V>) config);
		}
		else {
			createJCache((JCacheConfig<K, V>) config);
		}
	}

	private static boolean isUnInitialised() {
		return ((ehCacheManager == null) || 
					Status.UNINITIALIZED.equals(ehCacheManager.getStatus()) ||
					(jCacheManager == null) ||
					jCacheManager.isClosed());
	}
	
	@Override
	public PersistentCacheManager getEHCacheManager() {
		return ehCacheManager;
	}

	@Override
	public javax.cache.CacheManager getJCacheManager() {
		return jCacheManager;
	}

	@Override
	public <K extends Serializable, V extends Serializable> Cache<K, V> createEHCache(EHCacheConfig<K, V> config) {
		ResourcePoolsBuilder rpb = ResourcePoolsBuilder.newResourcePoolsBuilder();
		rpb = rpb.heap(config.getHeapSizeEntries(), EntryUnit.ENTRIES);
		long offHeapSizeInMB = config.getOffHeapSizeInMB();
		if (offHeapSizeInMB > 0) {
			rpb = rpb.offheap(offHeapSizeInMB, MemoryUnit.MB);
		}
		long diskSizeInMB = config.getDiskSizeInMB();
		if (diskSizeInMB > 0) {
			rpb = rpb.disk(diskSizeInMB, MemoryUnit.MB, config.isPersistent());
		}
		CacheConfigurationBuilder<K, V> ccb = CacheConfigurationBuilder.newCacheConfigurationBuilder(config.getKeyClass(), config.getValueClass(), rpb);
		CacheExpiryPolicy expiryPolicy = config.getExpiryPolicy();
		long expiryInMinutes = config.getExpiryInMinutes();
		if (CacheExpiryPolicy.eternal.equals(expiryPolicy)) {
			ccb = ccb.withExpiry(ExpiryPolicyBuilder.noExpiration());
		}
		else if (expiryInMinutes > 0) {
			if (CacheExpiryPolicy.timeToIdle.equals(expiryPolicy)) {
				ccb = ccb.withExpiry(ExpiryPolicyBuilder.timeToIdleExpiration(Duration.ofMinutes(expiryInMinutes)));
			}
			else if (CacheExpiryPolicy.timeToLive.equals(expiryPolicy)) {
				ccb = ccb.withExpiry(ExpiryPolicyBuilder.timeToLiveExpiration(Duration.ofMinutes(expiryInMinutes)));
			}
		}
		CacheConfiguration<K, V> cacheConfiguration = ccb.build();
		return ehCacheManager.createCache(config.getName(), cacheConfiguration); 
	}

	@Override
	@SuppressWarnings("resource")
	public <K extends Serializable, V extends Serializable> javax.cache.Cache<K, V> createJCache(JCacheConfig<K, V> config) {
		ResourcePoolsBuilder rpb = ResourcePoolsBuilder.heap(config.getHeapSizeEntries());
		long offHeapSizeInMB = config.getOffHeapSizeInMB();
		if (offHeapSizeInMB > 0) {
			rpb = rpb.offheap(offHeapSizeInMB, MemoryUnit.MB);
		}
		CacheConfigurationBuilder<K, V> ccb = CacheConfigurationBuilder.newCacheConfigurationBuilder(config.getKeyClass(), config.getValueClass(), rpb);
		CacheExpiryPolicy expiryPolicy = config.getExpiryPolicy();
		long expiryInMinutes = config.getExpiryInMinutes();
		if (CacheExpiryPolicy.eternal.equals(expiryPolicy)) {
			ccb = ccb.withExpiry(ExpiryPolicyBuilder.noExpiration());
		}
		else if (expiryInMinutes > 0) {
			if (CacheExpiryPolicy.timeToIdle.equals(expiryPolicy)) {
				ccb = ccb.withExpiry(ExpiryPolicyBuilder.timeToIdleExpiration(Duration.ofMinutes(expiryInMinutes)));
			}
			else if (CacheExpiryPolicy.timeToLive.equals(expiryPolicy)) {
				ccb = ccb.withExpiry(ExpiryPolicyBuilder.timeToLiveExpiration(Duration.ofMinutes(expiryInMinutes)));
			}
		}
		return getJCacheManager().createCache(config.getName(), Eh107Configuration.fromEhcacheCacheConfiguration(ccb));
	}

	@Override
	public void removeEHCache(String name) {
		ehCacheManager.removeCache(name);
	}

	@Override
	public void destroyEHCache(String name) throws CachePersistenceException {
		ehCacheManager.destroyCache(name);
	}

	@Override
	public final void destroyJCache(String name) {
		jCacheManager.destroyCache(name);
	}

	@Override
	public <K extends Object, V extends Object> Cache<K, V> getEHCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return ehCacheManager.getCache(name, keyClass, valueClass);
	}
	
	@Override
	public <K extends Object, V extends Object> javax.cache.Cache<K, V> getJCache(String name, Class<K> keyClass, Class<V> valueClass) {
		return jCacheManager.getCache(name, keyClass, valueClass);
	}
	
	@Override
	public CacheStatistics getEHCacheStatistics(String name) {
		CacheStatistics result = null;
		try {
			result = statisticsService.getCacheStatistics(name);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			UtilImpl.LOGGER.warning("Cache Stats requested on EHCache " + name + "that does not exist");
		}
		return result;
	}
	
	@Override
	public TierStatistics getEHTierStatistics(CacheStatistics statistics, CacheTier tier) {
		return (statistics == null) ? null : statistics.getTierStatistics().get(tier.toString());
	}
	
	@Override
	public CacheStatisticsMXBean getJCacheStatisticsMXBean(String name) {
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
