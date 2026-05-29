package org.skyve.impl.geoip;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Map;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.IPGeolocation;
import org.skyve.util.JSON;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

/**
 * Resolves IP geolocation data via the ipinfo.io API.
 *
 * <p>Not thread-safe by design: request execution is stateless but uses a per-call
 * {@link HttpClient} and does not share mutable state.
 */
public class IPInfoIo extends AbstractCachingGeoIPService {
	private static final String IPINFO_DOMAIN = "https://ipinfo.io/";

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(IPInfoIo.class);

	/**
	 * Looks up geolocation metadata for the given IP address.
	 *
	 * <p>Side effects: performs an outbound HTTPS call to ipinfo.io when the value
	 * is not already present in the inherited cache.
	 *
	 * @param ipAddress the IPv4 or IPv6 address to resolve
	 * @return the resolved geolocation, or {@link IPGeolocation#EMPTY} when lookup
	 *         fails or no usable location is returned
	 */
	@Override
	protected IPGeolocation doGeolocation(String ipAddress) {
		IPGeolocation result = IPGeolocation.EMPTY;
		
		HttpClient client = HttpClient.newHttpClient();
		String requestUrl = new StringBuilder(96).append(IPINFO_DOMAIN).append(ipAddress).append("/json?token=").append(UtilImpl.GEO_IP_KEY).toString();

		HttpRequest request = HttpRequest.newBuilder().uri(URI.create(requestUrl)).GET().build();

		try {
			HttpResponse<String> response = send(client, request);
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
				LOGGER.error("Error fetching data IP : {} : HTTP Error - {}", ipAddress, Integer.valueOf(response.statusCode()));
			}
		}
		catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			LOGGER.error("Error fetching data: IP {}", ipAddress, e);
		}
		catch (Exception e) {
			LOGGER.error("Error fetching data: IP {}", ipAddress, e);
		}
		
		return result;
	}

	/**
	 * Sends the given request using the provided HTTP client.
	 *
	 * <p>Exposed as a test seam so tests can stub network behaviour deterministically.
	 *
	 * @param client the HTTP client used to execute the request
	 * @param request the prepared request to send
	 * @return the HTTP response containing the ipinfo payload
	 * @throws IOException if request execution fails due to I/O errors
	 * @throws InterruptedException if the calling thread is interrupted while waiting
	 *         for the response
	 */
	@SuppressWarnings("static-method")
	protected HttpResponse<String> send(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
		return client.send(request, HttpResponse.BodyHandlers.ofString());
	}
}
