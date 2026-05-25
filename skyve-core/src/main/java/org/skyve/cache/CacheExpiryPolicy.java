package org.skyve.cache;

/**
 * Expiry policy for Skyve caches.
 *
 * <ul>
 *   <li>{@code timeToLive} — entry expires a fixed duration after it was created or last updated.</li>
 *   <li>{@code timeToIdle} — entry expires a fixed duration after it was last accessed.</li>
 *   <li>{@code eternal} — entry never expires (eviction is size-based only).</li>
 * </ul>
 *
 * @see CacheConfig
 */
public enum CacheExpiryPolicy {
	timeToLive, timeToIdle, eternal;
}
