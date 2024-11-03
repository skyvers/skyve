package org.skyve.cache;

import org.skyve.util.IPGeolocation;

public class GeoIPCacheConfig extends EHCacheConfig<String, IPGeolocation> {
	private static final String GEO_IPS_CACHE_NAME = "geoips";

	public GeoIPCacheConfig(long heapSizeEntries, long expiryInMinutes) {
		super(GEO_IPS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, IPGeolocation.class);
	}

	public GeoIPCacheConfig(long heapSizeEntries,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(GEO_IPS_CACHE_NAME, heapSizeEntries, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, IPGeolocation.class, diskSizeInMB);
	}

	public GeoIPCacheConfig(long heapSizeEntries,
									long offHeapSizeInMB,
									long diskSizeInMB,
									long expiryInMinutes) {
		super(GEO_IPS_CACHE_NAME, heapSizeEntries, offHeapSizeInMB, CacheExpiryPolicy.timeToIdle, expiryInMinutes, String.class, IPGeolocation.class, diskSizeInMB);
	}
	
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
