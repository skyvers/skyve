package modules.admin.UserLoginRecord;

import org.skyve.EXT;
import org.skyve.util.IPGeolocation;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import modules.admin.domain.UserLoginRecord;

public class UserLoginRecordExtension extends UserLoginRecord {
	private static final long serialVersionUID = 200018857407163578L;

	@Override
	public String getBizKey() {
		if (Boolean.TRUE.equals(getFailed())) {
			return "Failed Login attempt: " + super.getBizKey();
		}

		return super.getBizKey();
	}
	
	private IPGeolocation geoIP;
	
	/**
	 * Lazily geolocate the IP if present.
	 * This is not thread-safe but is good for use in a normal Skyve conversation
	 */
	public @Nonnull IPGeolocation getGeoIP() {
		if (geoIP == null) {
			geoIP = IPGeolocation.EMPTY;
			String ip = getIpAddress();
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

	/**
	 * Get the country name (for the current user's locale) for the country code.
	 */
	@Override
	public String getCountryName() {
		String countryCode = getCountryCode();
		return (countryCode == null) ? null : Util.countryNameFromCode(countryCode);
	}
}
