package util.deployed;

import java.net.URI;
import java.util.Collection;
import java.util.regex.Pattern;

/** Redacts deployed-test diagnostics before they are written to logs or result artifacts. */
public final class DeployedDiagnostics {
	private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
			"(?i)(password|passwd|secret|token|authorization|cookie)(\\s*[:=]\\s*)([^\\s,;]+)");

	private DeployedDiagnostics() {
		// Utility class.
	}

	/** Redacts common credential-bearing assignments and any exact protected values. */
	public static String redact(String diagnostic, Collection<String> protectedValues) {
		String result = SECRET_ASSIGNMENT.matcher(diagnostic).replaceAll("$1$2<redacted>");
		for (String value : protectedValues) {
			if ((value != null) && (! value.isEmpty())) {
				result = result.replace(value, "<redacted>");
			}
		}
		return result;
	}

	/** Removes user-info and query data while retaining a useful endpoint shape. */
	public static String endpointShape(URI uri) {
		int port = uri.getPort();
		return uri.getScheme() + "://" + uri.getHost() + ((port < 0) ? "" : ":" + port) + uri.getPath();
	}
}
