package org.skyve.impl.cdi;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Optional;

import org.skyve.impl.util.UtilImpl;

import jakarta.enterprise.context.ApplicationScoped;
import net.minidev.json.JSONObject;
import net.minidev.json.parser.JSONParser;

/**
 * Makes a request to the IPInfo (https://ipinfo.io/) web service to determine the
 * country for a visiting IP.
 */
@ApplicationScoped
public class GeoIpService {

	private static final String IPINFO_API_URL = "https://ipinfo.io/%s/json?token=%s";

	/**
	 * Looks up the country code for the specified IP address.
	 * 
	 * @param ipAddress The ip address to look up the country for
	 * @return The 2-letter country-code, or an empty optional if one could not be returned
	 */
	@SuppressWarnings("static-method")
	public Optional<String> getCountryCodeForIp(final String ipAddress) {
		HttpClient client = HttpClient.newHttpClient();
		String token = UtilImpl.IP_INFO_KEY;
		String requestUrl = String.format(IPINFO_API_URL, ipAddress, token);

		HttpRequest request = HttpRequest.newBuilder()
				.uri(URI.create(requestUrl))
				.GET()
				.build();

		try {
			HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
			if (response.statusCode() == 200) {

				String json = response.body();
				JSONParser parser = new JSONParser(JSONParser.MODE_JSON_SIMPLE);
				JSONObject body = (JSONObject) parser.parse(json);

				if ((body.getAsString("country") != null)) {
					return Optional.ofNullable(body.getAsString("country"));
				}
				System.err.println("Error fetching data: IP is probably invalid or unavailable in database");
			} else {
				System.err.println("HTTP Error: " + response.statusCode());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return Optional.empty();
	}
}