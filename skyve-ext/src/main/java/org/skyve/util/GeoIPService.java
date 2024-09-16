package org.skyve.util;

import org.locationtech.jts.geom.Point;
import org.skyve.impl.util.UtilImpl;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/*
 * A Geo IP service interface for Skyve.
 */
public interface GeoIPService {
	/**
	 * Result returned from a service call.
	 */
	public static record IPGeolocation (@Nullable String city,
											@Nullable String region,
											@Nullable String countryCode,
											@Nullable Point location) {
		/**
		 * Use this when a service is not in use or no valid data was returned from a service.
		 */
		public static IPGeolocation EMPTY = new IPGeolocation(null, null, null, null);
		
		/**
		 * Determine if the ipAddress should be blocked based on the country of origin.
		 */
		public boolean isBlocked() {
			boolean result = false;
			
			if (countryCode != null) {
				if (UtilImpl.GEO_IP_COUNTRY_CODES != null) {
					result = UtilImpl.GEO_IP_COUNTRY_CODES.contains(countryCode);
					if (UtilImpl.GEO_IP_WHITELIST) {
						result = ! result;
					}
				}
			}
			
			return result;
		}
	}
	
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
