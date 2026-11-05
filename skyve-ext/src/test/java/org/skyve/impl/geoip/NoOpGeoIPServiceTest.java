package org.skyve.impl.geoip;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.util.IPGeolocation;

class NoOpGeoIPServiceTest {

	@Test
	@SuppressWarnings("static-method")
	void testGeolocateReturnsEmpty() {
		// covers L14: geolocate always returns IPGeolocation.EMPTY
		NoOpGeoIPService service = new NoOpGeoIPService();
		assertSame(IPGeolocation.EMPTY, service.geolocate("1.2.3.4"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testIsBlockingReturnsFalse() {
		// covers L19: isBlocking always returns false
		NoOpGeoIPService service = new NoOpGeoIPService();
		assertFalse(service.isBlocking());
	}
}
