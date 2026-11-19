package org.skyve.impl.geoip;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.GeoIPService;

@SuppressWarnings("static-method")
class GeoIPServiceStaticSingletonTest {

        @SuppressWarnings("null") // passes null on purpose
        @Test
        void getReturnsNullInitially() {
                GeoIPServiceStaticSingleton.set(null);
                assertNull(GeoIPServiceStaticSingleton.get());
        }

        @SuppressWarnings("null") // passes null on purpose
        @Test
        void setAndGetRoundtrip() {
                GeoIPService svc = new NoOpGeoIPService();
                GeoIPServiceStaticSingleton.set(svc);
                try {
                        assertSame(svc, GeoIPServiceStaticSingleton.get());
                }
                finally {
                        GeoIPServiceStaticSingleton.set(null);
                }
        }

        @SuppressWarnings("null") // passes null on purpose
        @Test
        void setDefaultWithNullKeyInstallsNoOpService() {
                // UtilImpl.GEO_IP_KEY is null in test environment
                GeoIPServiceStaticSingleton.setDefault();
                try {
                        assertNotNull(GeoIPServiceStaticSingleton.get());
                }
                finally {
                        GeoIPServiceStaticSingleton.set(null);
                }
        }

        @SuppressWarnings("null") // passes null on purpose
        @Test
        void setDefaultWithNonNullKeyInstallsIPInfoIo() {
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
