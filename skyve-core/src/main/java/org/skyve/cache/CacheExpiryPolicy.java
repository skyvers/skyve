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
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum CacheExpiryPolicy {
	timeToLive, timeToIdle, eternal;
}
