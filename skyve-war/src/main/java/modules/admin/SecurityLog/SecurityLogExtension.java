package modules.admin.SecurityLog;

import org.skyve.EXT;
import org.skyve.util.IPGeolocation;

import jakarta.annotation.Nonnull;
import modules.admin.domain.SecurityLog;

/**
 * Extends {@link SecurityLog} with lazily-evaluated geolocation data derived
 * from the captured source IP address.
 *
 * <p>Threading: instances are request-scoped document instances and are not
 * designed for concurrent access.</p>
 */
public class SecurityLogExtension extends SecurityLog {
	private static final long serialVersionUID = -1140573043660368743L;

	private IPGeolocation geoIP;
	
	/**
	 * Returns the geolocation resolved from {@link #getSourceIP()}.
	 *
	 * <p>The lookup is performed once and cached for the lifetime of this bean
	 * instance.</p>
	 *
	 * @return the resolved geolocation, or {@link IPGeolocation#EMPTY} when no
	 *         source IP is available
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
	 * Indicates whether this log entry has a real geolocation result.
	 *
	 * @return {@code true} when geolocation data is available, otherwise
	 *         {@code false}
	 */
	@Override
	public boolean isHasLocation() {
		return (getGeoIP() != org.skyve.util.IPGeolocation.EMPTY);
	}
}
