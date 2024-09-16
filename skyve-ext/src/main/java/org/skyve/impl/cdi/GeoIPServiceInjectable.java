package org.skyve.impl.cdi;

import java.io.Serializable;

import org.skyve.EXT;
import org.skyve.util.GeoIPService;

import jakarta.enterprise.inject.Alternative;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class GeoIPServiceInjectable implements GeoIPService, Serializable {
	private static final long serialVersionUID = -5169979825841065030L;

	@Override
	public IPGeolocation geolocate(String ipAddress) {
		return EXT.getGeoIPService().geolocate(ipAddress);
	}
}
