package org.skyve.impl.geoip;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;

import org.junit.Test;
import org.skyve.util.IPGeolocation;

public class NoOpGeoIPServiceTest {

	@Test
	@SuppressWarnings("static-method")
	public void testGeolocateReturnsEmpty() {
		// covers L14: geolocate always returns IPGeolocation.EMPTY
		NoOpGeoIPService service = new NoOpGeoIPService();
		assertSame(IPGeolocation.EMPTY, service.geolocate("1.2.3.4"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testIsBlockingReturnsFalse() {
		// covers L19: isBlocking always returns false
		NoOpGeoIPService service = new NoOpGeoIPService();
		assertFalse(service.isBlocking());
	}
}
