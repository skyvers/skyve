package org.skyve.cache;

import org.skyve.util.IPGeolocation;

/**
 * Configures the GeoIP lookup cache.
 *
 * <p>Entries are keyed by IP address and hold resolved
 * {@link IPGeolocation} values with time-to-idle expiry.
 */
public class GeoIPCacheConfig extends EHCacheConfig<String, IPGeolocation> {
	private static final String GEO_IPS_CACHE_NAME = "geoips";

	/**
	 * Creates a heap-only GeoIP cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public GeoIPCacheConfig(long heapSizeEntries, long expiryInMinutes) {
		super(GEO_IPS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, IPGeolocation.class);
	}

	/**
	 * Creates a heap-plus-disk GeoIP cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public GeoIPCacheConfig(long heapSizeEntries,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(GEO_IPS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, IPGeolocation.class, diskSizeInMB);
	}

	/**
	 * Creates a heap/off-heap/disk GeoIP cache configuration.
	 *
	 * @param heapSizeEntries maximum heap entry count
	 * @param offHeapSizeInMB off-heap tier size in megabytes
	 * @param diskSizeInMB disk tier size in megabytes
	 * @param expiryInMinutes time-to-idle duration in minutes
	 */
	public GeoIPCacheConfig(long heapSizeEntries,
									long offHeapSizeInMB,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(GEO_IPS_CACHE_NAME, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, IPGeolocation.class, diskSizeInMB);
	}
	
	/**
	 * Returns a diagnostic representation of this configuration.
	 *
	 * @return config details in JSON-like form
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(128);
		sb.append("{heapSizeEntries:").append(getHeapSizeEntries());
		sb.append(",offHeapSizeInMB:").append(getOffHeapSizeInMB());
		sb.append(",diskSizeInMB:").append(getDiskSizeInMB());
		sb.append(",expiryInMinutes:").append(getExpiryInMinutes()).append('}');
		return sb.toString();
	}
}
