package modules.admin.SecurityLog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.IPGeolocation;

@SuppressWarnings("static-method")
public class SecurityLogExtensionTest {

	@Test
	void isHasLocationWithNullSourceIPReturnsFalse() {
		SecurityLogExtension log = new SecurityLogExtension();
		// getSourceIP() is null → geoIP stays EMPTY → isHasLocation = false
		assertFalse(log.isHasLocation());
	}

	@Test
	void getGeoIPWithNullSourceIPReturnsEmpty() {
		SecurityLogExtension log = new SecurityLogExtension();
		IPGeolocation result = log.getGeoIP();
		assertNotNull(result);
		assertEquals(IPGeolocation.EMPTY, result);
	}
}
