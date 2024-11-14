package org.skyve.impl.geoip;

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

/**
 * Client for the ipinfo.io geoip service.
 */
public class IPInfoIo extends AbstractCachingGeoIPService {
	private static final String IPINFO_DOMAIN = "https://ipinfo.io/";

	@Override
	protected IPGeolocation doGeolocation(String ipAddress) {
		IPGeolocation result = IPGeolocation.EMPTY;
		
		HttpClient client = HttpClient.newHttpClient();
		String requestUrl = new StringBuilder(96).append(IPINFO_DOMAIN).append(ipAddress).append("/json?token=").append(UtilImpl.GEO_IP_KEY).toString();

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
						location = new GeometryFactory().createPoint(new Coordinate(Double.parseDouble(loc.substring(0, commaIndex)),
																						Double.parseDouble(loc.substring(commaIndex + 1))));
					}
				}
				result = new IPGeolocation(city, region, countryCode, location);
			}
			else {
				UtilImpl.LOGGER.severe("Error fetching data IP : " + ipAddress + " : HTTP Error - " + response.statusCode());
			}
		}
		catch (Exception e) {
			UtilImpl.LOGGER.severe("Error fetching data: IP " + ipAddress);
			e.printStackTrace();
		}
		
		return result;
	}
}
