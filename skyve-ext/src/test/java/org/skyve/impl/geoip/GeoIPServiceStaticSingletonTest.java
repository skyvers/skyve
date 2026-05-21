package org.skyve.impl.geoip;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.impl.util.UtilImpl;
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

        @Test
        public void setDefaultWithNonNullKeyInstallsIPInfoIo() {
                String savedKey = UtilImpl.GEO_IP_KEY;
                UtilImpl.GEO_IP_KEY = "test-key";
                try {
                        GeoIPServiceStaticSingleton.setDefault();
                        GeoIPService svc = GeoIPServiceStaticSingleton.get();
                        assertNotNull(svc);
                        assertTrue(svc instanceof IPInfoIo);
                } finally {
                        UtilImpl.GEO_IP_KEY = savedKey;
                        GeoIPServiceStaticSingleton.set(null);
                }
        }
}
