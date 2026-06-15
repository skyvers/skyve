/**
 * Cache management API for EHCache and JCache (JSR-107) caches.
 *
 * <p>{@link org.skyve.cache.Caching} is the service facade for obtaining named EHCache
 * and JCache instances. Obtain it via {@link org.skyve.EXT#getCaching()}.
 * {@link org.skyve.cache.CacheTier} enumerates the available cache tier types
 * (heap, off-heap, disk) for configuration.
 *
 * @see org.skyve.cache.Caching
 */
package org.skyve.cache;
