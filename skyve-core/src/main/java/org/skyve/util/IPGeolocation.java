package org.skyve.util;

import java.io.Serializable;

import org.locationtech.jts.geom.Point;
import org.skyve.impl.util.UtilImpl;

import jakarta.annotation.Nullable;

/**
 * Result returned from a service call.
 */
public record IPGeolocation (@Nullable String city,
								@Nullable String region,
								@Nullable String countryCode,
								@Nullable Point location) implements Serializable {
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
