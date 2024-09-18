package modules.admin.UserLoginRecord;

import org.skyve.util.Util;

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
	
	/**
	 * Get the country name (for the current user's locale) for the country code.
	 */
	@Override
	public String getCountryName() {
		String countryCode = getCountryCode();
		return (countryCode == null) ? null : Util.countryNameFromCode(countryCode);
	}
}
