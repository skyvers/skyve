package org.skyve.impl.geoip;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.IPGeolocation;
import org.skyve.util.JSON;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Client for the ipinfo.io geoip service.
 */
public class IPInfoIo extends AbstractCachingGeoIPService {
	private static final String IPINFO_DOMAIN = "https://ipinfo.io/";

    private static final Logger LOGGER = LoggerFactory.getLogger(IPInfoIo.class);

	// Simple, conservative patterns for IPv4 and IPv6 literals.
	// These are intended to prevent URL path manipulation, not to be a complete IP parser.
	private static final Pattern IPV4_PATTERN = Pattern.compile(
			"^(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)" +
			"(\\.(25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)){3}$");

	private static final Pattern IPV6_PATTERN = Pattern.compile(
			"^([0-9a-fA-F]{1,4})(:([0-9a-fA-F]{1,4})){1,7}$");

	/**
	 * Validate and normalise an IP address string to ensure it cannot manipulate the request URL.
	 * Returns the trimmed IP if valid, or null if invalid.
	 */
	private static String sanitizeIpAddress(String ipAddress) {
		if (ipAddress == null) {
			return null;
		}
		String trimmed = ipAddress.trim();
		if (trimmed.isEmpty()) {
			return null;
		}

		// Reject characters that could break path or introduce query/fragment.
		if (trimmed.contains("/") || trimmed.contains("?") || trimmed.contains("#")) {
			return null;
		}

		Matcher ipv4Matcher = IPV4_PATTERN.matcher(trimmed);
		if (ipv4Matcher.matches()) {
			return trimmed;
		}

		Matcher ipv6Matcher = IPV6_PATTERN.matcher(trimmed);
		if (ipv6Matcher.matches()) {
			return trimmed;
		}

		return null;
	}

	@Override
	protected IPGeolocation doGeolocation(String ipAddress) {
		IPGeolocation result = IPGeolocation.EMPTY;

		// Sanitize the IP address derived from HTTP headers before using it in a URL.
		String safeIpAddress = sanitizeIpAddress(ipAddress);
		if (safeIpAddress == null) {
			LOGGER.warn("Skipping GeoIP lookup for invalid IP address value: {}", ipAddress);
			return result;
		}
		
		HttpClient client = HttpClient.newHttpClient();
		String requestUrl = new StringBuilder(96)
				.append(IPINFO_DOMAIN)
				.append(safeIpAddress)
				.append("/json?token=")
				.append(UtilImpl.GEO_IP_KEY)
				.toString();

		HttpRequest request = HttpRequest.newBuilder().uri(URI.create(requestUrl)).GET().build();

		try {
			HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
			if (response.statusCode() == 200) {
				String body = response.body();
				@SuppressWarnings("unchecked")
				Map<String, Object> json = (Map<String, Object>) JSON.unmarshall(body);
				String city = UtilImpl.processStringValue((String) json.get("city"));
				String region = UtilImpl.processStringValue((String) json.get("region"));
				String countryCode = UtilImpl.processStringValue((String) json.get("country"));
				Point location = null;
				String loc = UtilImpl.processStringValue((String) json.get("loc"));
				if (loc != null) {
					int commaIndex = loc.indexOf(',');
					if (commaIndex > -1) {
						location = new GeometryFactory().createPoint(new Coordinate(Double.parseDouble(loc.substring(commaIndex + 1)),
																						Double.parseDouble(loc.substring(0, commaIndex))));
					}
				}
				result = new IPGeolocation(city, region, countryCode, location);
			}
			else {
				LOGGER.error("Error fetching data IP : {} : HTTP Error - {}", safeIpAddress, response.statusCode());
			}
		}
		catch (Exception e) {
			LOGGER.error("Error fetching data: IP {}", safeIpAddress, e);
		}
		
		return result;
	}
}
