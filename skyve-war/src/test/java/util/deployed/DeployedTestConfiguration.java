package util.deployed;

import java.net.URI;
import java.time.Duration;
import java.util.Map;

import jakarta.annotation.Nullable;

/** Supplies environment-backed configuration shared by deployed integration test suites. */
public final class DeployedTestConfiguration {
	public static final URI DEFAULT_BASE_URL = URI.create("http://127.0.0.1:8080/skyve/");
	public static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(30);

	private final URI baseUrl;
	private final boolean headless;
	private final Duration timeout;
	private final String username;
	private final String password;

	private DeployedTestConfiguration(URI baseUrl,
									boolean headless,
									Duration timeout,
									String username,
									String password) {
		this.baseUrl = baseUrl;
		this.headless = headless;
		this.timeout = timeout;
		this.username = username;
		this.password = password;
	}

	/** Reads deployed-test settings from the process environment. */
	public static DeployedTestConfiguration fromEnvironment() {
		return from(System.getenv());
	}

	/** Builds configuration from an environment-shaped map for deterministic tests. */
	public static DeployedTestConfiguration from(Map<String, String> environment) {
		String baseUrl = value(environment, "SKYVE_DEPLOYED_IT_BASE_URL");
		String timeoutSeconds = value(environment, "SKYVE_DEPLOYED_IT_TIMEOUT_SECONDS");
		String headless = value(environment, "SKYVE_DEPLOYED_IT_HEADLESS");
		Duration timeout = (timeoutSeconds == null) ? DEFAULT_TIMEOUT : Duration.ofSeconds(parsePositive(timeoutSeconds));
		return new DeployedTestConfiguration((baseUrl == null) ? DEFAULT_BASE_URL : normalisedBaseUrl(baseUrl),
				(headless == null) || Boolean.parseBoolean(headless),
				timeout,
				value(environment, "SKYVE_DEPLOYED_IT_USERNAME"),
				value(environment, "SKYVE_DEPLOYED_IT_PASSWORD"));
	}

	public URI getBaseUrl() {
		return baseUrl;
	}

	public boolean isHeadless() {
		return headless;
	}

	public Duration getTimeout() {
		return timeout;
	}

	@Nullable
	public String getUsername() {
		return username;
	}

	@Nullable
	public String getPassword() {
		return password;
	}

	/** Resolves an application-relative destination against the configured base URL. */
	public URI resolve(String destination) {
		return baseUrl.resolve(destination);
	}

	private static URI normalisedBaseUrl(String value) {
		URI uri = URI.create(value);
		if ((! uri.isAbsolute()) || (uri.getHost() == null)) {
			throw new IllegalArgumentException("SKYVE_DEPLOYED_IT_BASE_URL must be an absolute HTTP(S) URL");
		}
		String scheme = uri.getScheme();
		if ((! "http".equalsIgnoreCase(scheme)) && (! "https".equalsIgnoreCase(scheme))) {
			throw new IllegalArgumentException("SKYVE_DEPLOYED_IT_BASE_URL must use HTTP or HTTPS");
		}
		return value.endsWith("/") ? uri : URI.create(value + '/');
	}

	private static long parsePositive(String value) {
		long result = Long.parseLong(value);
		if (result <= 0L) {
			throw new IllegalArgumentException("SKYVE_DEPLOYED_IT_TIMEOUT_SECONDS must be positive");
		}
		return result;
	}

	private static String value(Map<String, String> environment, String name) {
		String value = environment.get(name);
		if (value == null) {
			return null;
		}
		String result = value.trim();
		return result.isEmpty() ? null : result;
	}
}
