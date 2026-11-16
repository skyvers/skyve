package org.skyve.impl.geoip;

import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;

import jakarta.annotation.Nonnull;

/**
 * No GeoIP services.
 * This is set if there is no GEO IP service key set in the JSON.
 * This can also be used as a mock.
 */
public class NoOpGeoIPService implements GeoIPService {
	@Override
	public @Nonnull IPGeolocation geolocate(@Nonnull String ipAddress) {
		return IPGeolocation.EMPTY;
	}
	
	@Override
	public boolean isBlocking() {
		return false;
	}
}
