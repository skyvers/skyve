package org.skyve.impl.cache;

import java.io.File;
import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.time.Duration;
import java.util.Set;
import java.util.UUID;

import javax.cache.management.CacheStatisticsMXBean;
import javax.management.JMX;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import org.ehcache.Cache;
import org.ehcache.CacheManager;
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
import org.ehcache.core.internal.statistics.DefaultStatisticsService;
import org.ehcache.core.spi.service.StatisticsService;
import org.ehcache.core.statistics.CacheStatistics;
import org.ehcache.core.statistics.TierStatistics;
import org.ehcache.jsr107.Eh107Configuration;
import org.skyve.cache.CacheConfig;
import org.skyve.cache.CacheExpiryPolicy;
import org.skyve.cache.CacheTier;
import org.skyve.cache.Caching;
import org.skyve.cache.EHCacheConfig;
import org.skyve.cache.HibernateCacheConfig;
import org.skyve.cache.JCacheConfig;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.skyve.web.UserAgentType;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Singleton {@link org.skyve.cache.Caching} implementation that backs Skyve's
 * application-level caches with Infinispan/EhCache.
 */
@SuppressWarnings("java:S6548") // Singleton is fine.
public class DefaultCaching implements Caching {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(DefaultCaching.class);

	/**
	 * Names the framework-owned cache of physical user-agent detection results.
	 *
	 * <p>The region is internal rather than application-configurable so its memory bound and
	 * lifecycle cannot diverge from the request-processing assumptions in {@code UserAgent}.
	 */
	public static final String USER_AGENT_TYPE_CACHE_NAME = "org.skyve.internal.userAgentTypes";

	/**
	 * Limits the number of caller-controlled user-agent strings retained in heap memory.
	 *
	 * <p>A fixed entry bound prevents arbitrary {@code User-Agent} headers from creating an
	 * unbounded cache. Eviction is deliberately approximate; callers must not depend on a
	 * particular entry remaining cached.
	 */
	public static final long USER_AGENT_TYPE_CACHE_CAPACITY = 4096L;

	private static final DefaultCaching INSTANCE = new DefaultCaching();

	@SuppressWarnings("resource") // Owned and closed by shutdown().
	private PersistentCacheManager ehCacheManager;
	// This is a SPI class but there seems no clear way forward - see https://github.com/ehcache/ehcache3/issues/2951
	private StatisticsService statisticsService = new DefaultStatisticsService();
	@SuppressWarnings("resource") // Owned and closed by shutdown().
	private javax.cache.CacheManager jCacheManager;

	// This is either Util.getCacheDirectory(), or if multiple cache instances are required, Util.getCacheDirectory() + a random UUID.
	private String cacheDirectory;

	/**
	 * Prevent instantiation
	 */
	private DefaultCaching() {
		// nothing to see here
	}

	/**
	 * Returns the singleton instance.
	 */
	public static @Nonnull DefaultCaching get() {
		return INSTANCE;
	}

	/**
	 * Performs startup.
	 */
	@Override
	@SuppressWarnings({"resource", "java:S1143", "java:S1163", "java:S3776"}) // OK to throw in the finally block here as it stops deployment; Complexity OK
	public void startup() {
		if (isUnInitialised()) {
			try {
				shutdown(); // call this just in case the last deployment failed to get around ehcache's file lock.
			}
			finally {
				// Check if there are any persistent caches and multiple cache instances have been requested
				if (UtilImpl.CACHE_MULTIPLE) {
					for (CacheConfig<? extends Serializable, ? extends Serializable> config : UtilImpl.APP_CACHES) {
						if (config instanceof EHCacheConfig<?, ?> ehConfig && ehConfig.isPersistent()) {
							throw new IllegalStateException("Cannot run multiple cache instances when one of the caches is persistent");
						}
					}
				}

				cacheDirectory = Util.getCacheDirectory();
				if (UtilImpl.CACHE_MULTIPLE) {
					cacheDirectory += ProcessHandle.current().pid() + "-" + UUID.randomUUID().toString() + "/";
				}
				if (UtilImpl.FORCE_NON_PERSISTENT_CACHING) {
					CacheManager cm = CacheManagerBuilder.newCacheManagerBuilder()
															.using(statisticsService)
															.build(true);
					ehCacheManager = new NonPersistentCacheManager(cm);
				}
				else {
					ehCacheManager = CacheManagerBuilder.newCacheManagerBuilder()
										.using(statisticsService)
										.with(CacheManagerBuilder.persistence(cacheDirectory))
										.build(true);
				}
				jCacheManager = javax.cache.Caching.getCachingProvider().getCacheManager();

				createUserAgentTypeCache();

				// Create the conversations cache
				if (UtilImpl.CONVERSATION_CACHE != null) {
					LOGGER.info("Create the conversation cache with config {}", UtilImpl.CONVERSATION_CACHE);
					createEHCache(UtilImpl.CONVERSATION_CACHE);
				}

				// Create the CSRF Token cache
				if (UtilImpl.CSRF_TOKEN_CACHE != null) {
					LOGGER.info("Create the CSRF token cache with config {}", UtilImpl.CSRF_TOKEN_CACHE);
					createEHCache(UtilImpl.CSRF_TOKEN_CACHE);
				}

				// Create the Geo IP cache
				if (UtilImpl.GEO_IP_CACHE != null) {
					LOGGER.info("Create the Geo IP cache with config {}", UtilImpl.GEO_IP_CACHE);
					createEHCache(UtilImpl.GEO_IP_CACHE);
				}

				// Create the sessions cache
				if (UtilImpl.SESSION_CACHE != null) {
					LOGGER.info("Create the session cache with config {}", UtilImpl.SESSION_CACHE);
					createEHCache(UtilImpl.SESSION_CACHE);
				}

				// Create the app caches
				for (CacheConfig<? extends Serializable, ? extends Serializable> config : UtilImpl.APP_CACHES) {
					LOGGER.info("Create app cache with config {}", config);
					createCache(config);
				}

				// Create the hibernate caches
				for (HibernateCacheConfig config : UtilImpl.HIBERNATE_CACHES) {
					LOGGER.info("Create hibernate cache with config {}", config);
					createJCache(config);
				}
			}
		}
	}

	/**
	 * Creates the bounded, concurrent cache used for physical user-agent detection.
	 *
	 * <p>Rationale: detection results are read on the request hot path, so Ehcache permits
	 * concurrent access without serialising every request on one application object monitor.
	 * A plain {@link java.util.concurrent.ConcurrentHashMap} was not selected because the HTTP
	 * {@code User-Agent} header is caller-controlled and therefore requires a reliable memory
	 * bound. This region uses the existing shared Ehcache manager instead of creating a private
	 * manager, keeping startup, shutdown, statistics and resource ownership within the framework's
	 * established cache lifecycle.
	 *
	 * <p>Eviction is Ehcache's concurrent approximate policy, not an exact access-order guarantee.
	 * A cache miss can consequently repeat detection but cannot change its result.
	 *
	 * @implNote The region is heap-only and eternal: the entry limit controls retention, while
	 * expiry timers and persistent storage would add overhead without improving detection
	 * correctness.
	 */
	private void createUserAgentTypeCache() {
		createEHCache(new EHCacheConfig<>(USER_AGENT_TYPE_CACHE_NAME,
											USER_AGENT_TYPE_CACHE_CAPACITY,
											CacheExpiryPolicy.eternal,
											0L,
											String.class,
											UserAgentType.class));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void shutdown() {
		// NB all caches are closed by closing the cache managers
		if ((ehCacheManager != null) && (! Status.UNINITIALIZED.equals(ehCacheManager.getStatus()))) {
			try {
				ehCacheManager.close();
			}
			finally {
				if (UtilImpl.CACHE_MULTIPLE) {
					try {
						ehCacheManager.destroy();
					}
					catch (CachePersistenceException e) {
						LOGGER.warn("Could not remove the cache files/folders", e);
					}
					finally {
						try {
							File folder = new File(cacheDirectory);
							if (folder.exists()) {
								FileUtil.delete(folder);
							}
						}
						catch (Exception e) { // IO and NPE
							LOGGER.warn("Could not delete the cache folder {}", cacheDirectory, e);
						}
					}
				}
				ehCacheManager = null;
			}
		}
		if ((jCacheManager != null) && (! jCacheManager.isClosed())) {
			jCacheManager.close();
			jCacheManager = null;
		}
	}

	@SuppressWarnings("resource")
	private <K extends Serializable, V extends Serializable> void createCache(@Nonnull CacheConfig<K, V> config) {
		if (config instanceof EHCacheConfig<?, ?>) {
			createEHCache((EHCacheConfig<K, V>) config);
		}
		else {
			createJCache((JCacheConfig<K, V>) config);
		}
	}

	private boolean isUnInitialised() {
		return ((ehCacheManager == null) ||
					Status.UNINITIALIZED.equals(ehCacheManager.getStatus()) ||
					(jCacheManager == null) ||
					jCacheManager.isClosed());
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull PersistentCacheManager getEHCacheManager() {
		return ehCacheManager;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull javax.cache.CacheManager getJCacheManager() {
		return jCacheManager;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <K extends Serializable, V extends Serializable> Cache<K, V> createEHCache(@Nonnull EHCacheConfig<K, V> config) {
		ResourcePoolsBuilder rpb = ResourcePoolsBuilder.newResourcePoolsBuilder();
		rpb = rpb.heap(config.getHeapSizeEntries(), EntryUnit.ENTRIES);
		if (! UtilImpl.FORCE_NON_PERSISTENT_CACHING) {
			long offHeapSizeInMB = config.getOffHeapSizeInMB();
			if (offHeapSizeInMB > 0) {
				rpb = rpb.offheap(offHeapSizeInMB, MemoryUnit.MB);
			}
			long diskSizeInMB = config.getDiskSizeInMB();
			if (diskSizeInMB > 0) {
				rpb = rpb.disk(diskSizeInMB, MemoryUnit.MB, config.isPersistent());
			}
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

	/**
	 * Creates the jCache.
	 */
	@Override
	@SuppressWarnings("resource")
	public @Nonnull <K extends Serializable, V extends Serializable> javax.cache.Cache<K, V> createJCache(@Nonnull JCacheConfig<K, V> config) {
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
		return jCacheManager.createCache(config.getName(), Eh107Configuration.fromEhcacheCacheConfiguration(ccb));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeEHCache(@Nonnull String name) {
		ehCacheManager.removeCache(name);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void destroyEHCache(@Nonnull String name) throws CachePersistenceException {
		ehCacheManager.destroyCache(name);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void destroyJCache(@Nonnull String name) {
		jCacheManager.destroyCache(name);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean isEHCache(@Nonnull String name) {
		return ehCacheManager.getRuntimeConfiguration().getCacheConfigurations().containsKey(name);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull <K extends Object, V extends Object> Cache<K, V> getEHCache(@Nonnull String name,
																					@Nonnull Class<K> keyClass,
																					@Nonnull Class<V> valueClass) {
		Cache<K, V> result = ehCacheManager.getCache(name, keyClass, valueClass);
		if (result == null) {
			throw new DomainException("EHCache " + name + " does not exist");
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("resource") // The shared caching service owns the manager lifecycle.
	public boolean isJCache(@Nonnull String name) {
		return jCacheManager.getCache(name) != null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("resource") // The manager retains cache ownership.
	public @Nonnull <K extends Object, V extends Object> javax.cache.Cache<K, V> getJCache(@Nonnull String name,
																							@Nonnull Class<K> keyClass,
																							@Nonnull Class<V> valueClass) {
		javax.cache.Cache<K, V> result = jCacheManager.getCache(name, keyClass, valueClass);
		if (result == null) {
			throw new DomainException("JCache " + name + " does not exist");
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nonnull CacheStatistics getEHCacheStatistics(@Nonnull String name) {
		CacheStatistics result;
		try {
			result = statisticsService.getCacheStatistics(name);
		}
		catch (Exception e) {
			throw new DomainException("Statistics requested for EHCache " + name + " that does not exist", e);
		}
		if (result == null) {
			throw new DomainException("Statistics requested for EHCache " + name + " that does not exist");
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nullable TierStatistics getEHTierStatistics(@Nonnull CacheStatistics statistics, @Nonnull CacheTier tier) {
		return statistics.getTierStatistics().get(tier.toString());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public @Nullable CacheStatisticsMXBean getJCacheStatisticsMXBean(@Nonnull String name) {
		final MBeanServer mbeanServer = ManagementFactory.getPlatformMBeanServer();
		ObjectName objectName = null;
		try {
			objectName = new ObjectName("*:type=CacheStatistics,*,Cache=" + name);
		}
		catch (MalformedObjectNameException e) {
			LOGGER.error("Could not create statistics object name for cache {}", name, e);
		}
		Set<ObjectName> beans = mbeanServer.queryNames(objectName, null);
		if (beans.isEmpty()) {
			return null;
		}
		ObjectName[] objArray = beans.toArray(new ObjectName[beans.size()]);
		return JMX.newMBeanProxy(mbeanServer, objArray[0], CacheStatisticsMXBean.class);
	}
}
