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
import java.util.Map;

/** Submits direct normal and PrimeFaces Ajax postbacks with an authenticated browser session. */
public final class DeployedPostbackClient {
	private final HttpClient client;
	private final Duration timeout;

	public DeployedPostbackClient(DeployedTestConfiguration configuration, List<HttpCookie> authenticatedCookies) {
		timeout = configuration.getTimeout();
		CookieManager cookies = new CookieManager();
		for (HttpCookie cookie : authenticatedCookies) {
			cookies.getCookieStore().add(configuration.getBaseUrl(), cookie);
		}
		client = HttpClient.newBuilder().cookieHandler(cookies).connectTimeout(timeout).build();
	}

	/** Submits a full JSF form postback and returns the response body and headers. */
	public HttpResponse<String> submitNormal(URI action, Map<String, String> values)
	throws IOException, InterruptedException {
		return submit(action, values, false);
	}

	/** Submits a PrimeFaces partial Ajax postback and returns the partial response document. */
	public HttpResponse<String> submitAjax(URI action, Map<String, String> values)
	throws IOException, InterruptedException {
		return submit(action, values, true);
	}

	private HttpResponse<String> submit(URI action, Map<String, String> values, boolean ajax)
	throws IOException, InterruptedException {
		HttpRequest.Builder request = HttpRequest.newBuilder(action).timeout(timeout)
				.header("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
				.POST(HttpRequest.BodyPublishers.ofString(JsfPostbackSupport.encode(values)));
		if (ajax) {
			request.header("Faces-Request", "partial/ajax");
		}
		return client.send(request.build(), HttpResponse.BodyHandlers.ofString());
	}
}
