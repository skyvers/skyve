package org.skyve.impl.cdi;

import java.io.Serializable;

import org.skyve.EXT;
import org.skyve.util.GeoIPService;
import org.skyve.util.IPGeolocation;

import jakarta.enterprise.inject.Alternative;

/**
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 * 
 * @author mike
 */
@Alternative
public class GeoIPServiceInjectable implements GeoIPService, Serializable {
	private static final long serialVersionUID = 2567279857426720449L;

	@Override
	public IPGeolocation geolocate(String ipAddress) {
		return EXT.getGeoIPService().geolocate(ipAddress);
	}
}
