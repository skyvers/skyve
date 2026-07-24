package util.deployed;

import java.io.IOException;
import java.net.CookieManager;
import java.net.HttpCookie;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Reads the authenticated deployed-overlay probe through one correlation-scoped client. */
public final class DeployedProbeClient {
	private static final Pattern TOKEN = Pattern.compile("\\\"token\\\":\\\"([^\\\"]+)\\\"");
	private final URI probeUri;
	private final HttpClient client;
	private final Duration timeout;

	public DeployedProbeClient(DeployedTestConfiguration configuration, List<HttpCookie> authenticatedCookies) {
		probeUri = configuration.resolve("deployed-it/probe");
		timeout = configuration.getTimeout();
		CookieManager cookies = new CookieManager();
		for (HttpCookie cookie : authenticatedCookies) {
			cookies.getCookieStore().add(configuration.getBaseUrl(), cookie);
		}
		client = HttpClient.newBuilder().cookieHandler(cookies).connectTimeout(timeout).build();
	}

	/** Starts a server-authorised correlation and returns its one-time read token. */
	public DeployedCorrelation start(String correlationId) throws IOException, InterruptedException {
		HttpRequest request = HttpRequest.newBuilder(probeUri).timeout(timeout)
				.header("X-Skyve-Deployed-It-Correlation", correlationId).POST(HttpRequest.BodyPublishers.noBody()).build();
		HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
		ensureSuccess(response);
		Matcher token = TOKEN.matcher(response.body());
		if (! token.find()) {
			throw new IOException("Probe response did not contain a correlation token");
		}
		return new DeployedCorrelation(correlationId, token.group(1));
	}

	/** Returns the raw generic event document for suite-owned assertions. */
	public String read(DeployedCorrelation correlation) throws IOException, InterruptedException {
		HttpRequest.Builder request = HttpRequest.newBuilder(probeUri).timeout(timeout).GET();
		correlation.headers().forEach(request::header);
		HttpResponse<String> response = client.send(request.build(), HttpResponse.BodyHandlers.ofString());
		ensureSuccess(response);
		return response.body();
	}

	/** Clears server state after a suite completes. */
	public void clear(DeployedCorrelation correlation) throws IOException, InterruptedException {
		HttpRequest.Builder request = HttpRequest.newBuilder(probeUri).timeout(timeout).DELETE();
		correlation.headers().forEach(request::header);
		HttpResponse<Void> response = client.send(request.build(), HttpResponse.BodyHandlers.discarding());
		if (response.statusCode() != 204) {
			throw new IOException("Probe returned HTTP " + response.statusCode());
		}
	}

	private static void ensureSuccess(HttpResponse<?> response) throws IOException {
		if ((response.statusCode() < 200) || (response.statusCode() >= 300)) {
			throw new IOException("Probe returned HTTP " + response.statusCode());
		}
	}
}
