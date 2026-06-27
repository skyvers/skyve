/**
 * GeoIP service implementations for resolving IP addresses to geographic locations.
 *
 * <p>{@code AbstractCachingGeoIPService} provides a caching base layer.
 * {@code IPInfoIo} calls the ipinfo.io REST API. {@code NoOpGeoIPService}
 * is a no-operation stub. {@code GeoIPServiceStaticSingleton} holds the
 * configured runtime instance.
 */
package org.skyve.impl.geoip;
