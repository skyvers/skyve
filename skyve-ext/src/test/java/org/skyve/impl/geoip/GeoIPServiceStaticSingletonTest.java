package org.skyve.impl.geoip;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.junit.Test;
import org.skyve.util.GeoIPService;

@SuppressWarnings("static-method")
public class GeoIPServiceStaticSingletonTest {

	@Test
	public void getReturnsNullInitially() {
		GeoIPServiceStaticSingleton.set(null);
		assertNull(GeoIPServiceStaticSingleton.get());
	}

	@Test
	public void setAndGetRoundtrip() {
		GeoIPService svc = new NoOpGeoIPService();
		GeoIPServiceStaticSingleton.set(svc);
		try {
			assertSame(svc, GeoIPServiceStaticSingleton.get());
		}
		finally {
			GeoIPServiceStaticSingleton.set(null);
		}
	}

	@Test
	public void setDefaultWithNullKeyInstallsNoOpService() {
		// UtilImpl.GEO_IP_KEY is null in test environment
		GeoIPServiceStaticSingleton.setDefault();
		try {
			assertNotNull(GeoIPServiceStaticSingleton.get());
		}
		finally {
			GeoIPServiceStaticSingleton.set(null);
		}
	}
}
