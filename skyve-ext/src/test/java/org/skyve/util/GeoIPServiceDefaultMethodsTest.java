package org.skyve.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.CopyOnWriteArraySet;

import org.junit.Test;
import org.skyve.impl.util.UtilImpl;

/**
 * Tests the default methods of the {@link GeoIPService} interface.
 * Uses a minimal anonymous implementation of the interface.
 */
@SuppressWarnings("static-method")
public class GeoIPServiceDefaultMethodsTest {

        /** Minimal implementation for testing default methods. */
        private static final GeoIPService MINIMAL = ipAddress -> IPGeolocation.EMPTY;

        @Test
        public void isBlockingReturnsFalseWhenCountryCodesIsNull() {
                // UtilImpl.GEO_IP_COUNTRY_CODES is null by default in unit test env
                assertFalse(MINIMAL.isBlocking());
        }

        @Test
        public void isBlockingReturnsTrueWhenCountryCodesIsSet() {
                UtilImpl.GEO_IP_COUNTRY_CODES = new CopyOnWriteArraySet<>();
                try {
                        assertTrue(MINIMAL.isBlocking());
                } finally {
                        UtilImpl.GEO_IP_COUNTRY_CODES = null;
                }
        }

        @Test
        public void isWhitelistReturnsTrueByDefault() {
                // UtilImpl.GEO_IP_WHITELIST is true by default in unit test env
                assertTrue(MINIMAL.isWhitelist());
        }

        @Test
        public void isWhitelistReturnsFalseWhenSetToFalse() {
                UtilImpl.GEO_IP_WHITELIST = false;
                try {
                        assertFalse(MINIMAL.isWhitelist());
                } finally {
                        UtilImpl.GEO_IP_WHITELIST = true;
                }
        }
}
