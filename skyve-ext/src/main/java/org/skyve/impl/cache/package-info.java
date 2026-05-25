/**
 * Caching abstraction and HTTP-session state management for Skyve applications.
 *
 * <p>{@code DefaultCaching} is the singleton {@link org.skyve.cache.Caching}
 * implementation backed by Infinispan/EhCache. {@code NonPersistentCacheManager}
 * wraps a non-persistent cache manager. {@code StateUtil} manages serialised
 * conversation state in the HTTP session.
 */
package org.skyve.impl.cache;
