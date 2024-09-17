package org.skyve.impl.geoip;

import org.ehcache.Cache;
import org.skyve.impl.cache.StateUtil;
import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;

import jakarta.annotation.Nonnull;

/**
 * Extend this if your GeoIPService requires caching as set in the JSON config under state.geoIPs.
 */
public abstract class AbstractCachingGeoIPService implements GeoIPService {
	/**
	 * Overridden to hit the cache first, and call doGeolocation() if its a miss.
	 */
	@Override
	public final IPGeolocation geolocate(String ipAddress) {
		Cache<String, IPGeolocation> cache = StateUtil.getGeoIPs();
		IPGeolocation result = cache.get(ipAddress);
		if (result == null) {
			result = doGeolocation(ipAddress);
			cache.put(ipAddress, result);
		}
		return result;
	}
	
	/**
	 * Geo-locates details for the specified IP address.
	 * This is to be implemented by an extending class.
	 * 
	 * @param ipAddress The ip address to look up the country for
	 * @return The 2-letter country-code, or null if one could not be returned
	 */
	protected abstract @Nonnull IPGeolocation doGeolocation(@Nonnull String ipAddress);
}
