package org.skyve.impl.geoip;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.GeoIPService;

import jakarta.annotation.Nonnull;

/**
 * A singleton for the geo IP service to use.
 * NB This doesn't need to be thread safe as it is set at startup only.
 */
public class GeoIPServiceStaticSingleton {
	private static GeoIPService instance;
	
	private GeoIPServiceStaticSingleton() {
		// nothing to see here
	}
	
	public static @Nonnull GeoIPService get() {
		return instance;
	}
	
	public static void set(@Nonnull GeoIPService instance) {
		GeoIPServiceStaticSingleton.instance = instance;
	}

	public static void setDefault() {
		instance = (UtilImpl.GEO_IP_KEY == null) ? new NoOpGeoIPService() :  new IPInfoIo();
	}
}
