package util.deployed;

import java.security.SecureRandom;
import java.util.HexFormat;
import java.util.Map;

/** Carries the server-issued probe token for one deployed-test correlation. */
public record DeployedCorrelation(String correlationId, String token) {
	private static final SecureRandom RANDOM = new SecureRandom();

	/** Creates a non-secret correlation identifier suitable for logs and result paths. */
	public static String newCorrelationId() {
		byte[] random = new byte[16];
		RANDOM.nextBytes(random);
		return "dit-" + HexFormat.of().formatHex(random);
	}

	/** Returns headers required to read or clear the authenticated probe. */
	public Map<String, String> headers() {
		return Map.of("X-Skyve-Deployed-It-Correlation", correlationId,
				"X-Skyve-Deployed-It-Token", token);
	}
}
