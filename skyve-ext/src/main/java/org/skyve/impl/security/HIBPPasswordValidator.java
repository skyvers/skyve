package org.skyve.impl.security;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

/**
 * REST client to pass new passwords to <code>HaveIBeenPwned</code> API.
 * 
 * @author Simeon Solomou
 */
public class HIBPPasswordValidator {

	private static final String HIBP_API_URL = "https://api.pwnedpasswords.com/range/";

	/**
	 * Returns true if the passed password has been breached, using <code>IHaveBeenPwned</code> API.
	 * 
	 * @param password The password to validate
	 * @return true if password has been pwned
	 * @throws Exception
	 */
	public static boolean isPasswordPwned(@Nonnull String password) {
		try {
			// Hash the password
			String sha1Hash = hashPassword(password);

			// Get prefix
			String prefix = sha1Hash.substring(0, 5);

			// Query the HaveIBeenPwned API with the prefix
			HttpClient client = HttpClient.newHttpClient();
			HttpRequest request = HttpRequest.newBuilder()
					.uri(URI.create(HIBP_API_URL + prefix))
					.GET()
					.build();

			HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

			// Check if the hash suffix is present in the response
			String body = response.body();
			String suffix = sha1Hash.substring(5)
					.toUpperCase();
			
			return isHashSuffixPresent(body, suffix);
		} catch (Exception e) {
			Util.LOGGER.warning("HaveIBeenPwned API call failed");
			e.printStackTrace();
		}

		return false;
	}

	/**
	 * Returns a SHA-1 hash of the passed password in hexadecimal format.
	 * 
	 * @param password
	 * @return hashed password
	 * @throws NoSuchAlgorithmException
	 */
	private static String hashPassword(String password) throws NoSuchAlgorithmException {
		MessageDigest digest = MessageDigest.getInstance("SHA-1");
		byte[] hashBytes = digest.digest(password.getBytes(StandardCharsets.UTF_8));
		StringBuilder hexString = new StringBuilder();
		for (byte b : hashBytes) {
			String hex = Integer.toHexString(0xff & b);
			if (hex.length() == 1)
				hexString.append('0');
			hexString.append(hex);
		}
		return hexString.toString()
				.toUpperCase();
	}

	/**
	 * Returns true if the passed <code>suffix</code> matches any of those in the API <code>response</code>.
	 * 
	 * @param response List of hash suffixes
	 * @param suffix Suffix to match
	 * @return true if there is a match
	 */
	private static boolean isHashSuffixPresent(String response, String suffix) {
		String[] lines = response.split("\r\n");
		for (String line : lines) {
			if (line.startsWith(suffix)) {
				return true;
			}
		}
		return false;
	}
}
