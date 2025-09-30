package modules.admin.SecurityLog;

import org.skyve.EXT;
import org.skyve.util.IPGeolocation;

import jakarta.annotation.Nonnull;
import modules.admin.domain.SecurityLog;

public class SecurityLogExtension extends SecurityLog {
	private static final long serialVersionUID = -1140573043660368743L;

	private IPGeolocation geoIP;
	
	/**
	 * Lazily geolocate the IP if present.
	 * This is not thread-safe but is good for use in a normal Skyve conversation
	 */
	public @Nonnull IPGeolocation getGeoIP() {
		if (geoIP == null) {
			geoIP = IPGeolocation.EMPTY;
			String ip = getSourceIP();
			if (ip != null) {
				geoIP = EXT.getGeoIPService().geolocate(ip);
			}
		}
		return geoIP;
	}
	
	/**
	 * Overridden to check if we have IP geo-location data
	 */
	@Override
	public boolean isHasLocation() {
		return (getGeoIP() != org.skyve.util.IPGeolocation.EMPTY);
	}
}
