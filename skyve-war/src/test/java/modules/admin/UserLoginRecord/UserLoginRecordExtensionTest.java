package modules.admin.UserLoginRecord;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.IPGeolocation;

@SuppressWarnings("static-method")
public class UserLoginRecordExtensionTest {

	@Test
	void getCountryNameWithNullCountryCodeReturnsNull() {
		UserLoginRecordExtension record = new UserLoginRecordExtension();
		assertNull(record.getCountryName());
	}

	@Test
	void isHasLocationWithNullIpAddressReturnsFalse() {
		UserLoginRecordExtension record = new UserLoginRecordExtension();
		// getIpAddress() is null → geoIP stays EMPTY → isHasLocation = false
		assertFalse(record.isHasLocation());
	}

	@Test
	void getGeoIPWithNullIpAddressReturnsEmpty() {
		UserLoginRecordExtension record = new UserLoginRecordExtension();
		IPGeolocation result = record.getGeoIP();
		assertNotNull(result);
		assertEquals(IPGeolocation.EMPTY, result);
	}

	@Test
	void getBizKeyWithFailedTruePrependsPrefix() {
		UserLoginRecordExtension record = new UserLoginRecordExtension();
		record.setFailed(Boolean.TRUE);
		String bizKey = record.getBizKey();
		assertNotNull(bizKey);
		assertEquals("Failed Login attempt: " + record.getBizKey().replace("Failed Login attempt: ", ""), bizKey);
	}
}
