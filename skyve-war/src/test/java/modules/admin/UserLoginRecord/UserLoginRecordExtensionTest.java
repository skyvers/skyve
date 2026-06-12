package modules.admin.UserLoginRecord;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.IPGeolocation;

@SuppressWarnings("static-method")
class UserLoginRecordExtensionTest {

	@Test
	void getCountryNameWithNullCountryCodeReturnsNull() {
		UserLoginRecordExtension loginRecord = new UserLoginRecordExtension();
		assertNull(loginRecord.getCountryName());
	}

	@Test
	void isHasLocationWithNullIpAddressReturnsFalse() {
		UserLoginRecordExtension loginRecord = new UserLoginRecordExtension();
		// getIpAddress() is null → geoIP stays EMPTY → isHasLocation = false
		assertFalse(loginRecord.isHasLocation());
	}

	@Test
	void getGeoIPWithNullIpAddressReturnsEmpty() {
		UserLoginRecordExtension loginRecord = new UserLoginRecordExtension();
		IPGeolocation result = loginRecord.getGeoIP();
		assertNotNull(result);
		assertEquals(IPGeolocation.EMPTY, result);
	}

	@Test
	void getBizKeyWithFailedTruePrependsPrefix() {
		UserLoginRecordExtension loginRecord = new UserLoginRecordExtension();
		loginRecord.setFailed(Boolean.TRUE);
		String bizKey = loginRecord.getBizKey();
		assertNotNull(bizKey);
		assertEquals("Failed Login attempt: " + loginRecord.getBizKey().replace("Failed Login attempt: ", ""), bizKey);
	}
}
