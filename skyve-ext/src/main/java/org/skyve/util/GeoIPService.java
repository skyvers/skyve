package org.skyve.util;

import org.skyve.impl.util.UtilImpl;

import jakarta.annotation.Nonnull;

/**
 * A Geo IP service interface for Skyve.
 */
public interface GeoIPService {
	/**
	 * Geo-locates details for the specified IP address.
	 * 
	 * @param ipAddress The ip address to look up the country for
	 * @return The 2-letter country-code, or null if one could not be returned
	 */
	@Nonnull IPGeolocation geolocate(@Nonnull String ipAddress);
	
	/**
	 * Indicate if this service can be used to block IP addresses.
	 * @return true for blocking or false for not
	 */
	default boolean isBlocking() {
		return UtilImpl.GEO_IP_COUNTRY_CODES != null;
	}

	/**
	 * Indicate if whitelist or blacklist.
	 * @return true for whitelist or false for blacklist
	 */
	default boolean isWhitelist() {
		return UtilImpl.GEO_IP_WHITELIST;
	}
}
