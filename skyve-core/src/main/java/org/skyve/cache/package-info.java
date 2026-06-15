/**
 * Cache configuration value types for Skyve's EHCache and JCache (JSR-107) integration.
 *
 * <p>{@link org.skyve.cache.CacheConfig} is the abstract base configuration. Concrete
 * subclasses include {@link org.skyve.cache.EHCacheConfig} (EHCache-specific options
 * such as off-heap tier sizing) and {@link org.skyve.cache.JCacheConfig} (JSR-107
 * configuration for clustered environments).
 *
 * <p>Pre-built configurations for well-known Skyve caches are provided as concrete
 * subclasses: {@link org.skyve.cache.ConversationCacheConfig},
 * {@link org.skyve.cache.SessionCacheConfig}, {@link org.skyve.cache.CSRFTokenCacheConfig},
 * {@link org.skyve.cache.GeoIPCacheConfig}, {@link org.skyve.cache.HibernateCacheConfig}, and
 * {@link org.skyve.cache.ArchivedDocumentCacheConfig}.
 *
 * <p>The cache management service API is {@link org.skyve.cache.Caching} (in {@code skyve-ext}).
 *
 * @see org.skyve.cache.CacheConfig
 * @see org.skyve.cache.Caching
 */
package org.skyve.cache;
