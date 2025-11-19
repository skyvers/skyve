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
	public static final IPGeolocation EMPTY = new IPGeolocation(null, null, null, null);
	
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
	
	/**
	 * Get the country name of the Geolocated IP in the current user's locale.
	 * @return	The country name in the current user's locale, or if no user, the default system locale, or null if unknown. 
	 */
	public @Nullable String getCountry() {
		String result = null;
		if (countryCode != null) {
			result = Util.countryNameFromCode(countryCode);
		}
		return result;
	}
	
	/**
	 * Keep EMPTY reference equivalent
	 * @return this or EMPTY
	 */
	private Object readResolve() {
		if ((city == null) && (region == null) && (countryCode == null) && (location == null)) {
			return EMPTY;
		}
		return this;
	}
}
