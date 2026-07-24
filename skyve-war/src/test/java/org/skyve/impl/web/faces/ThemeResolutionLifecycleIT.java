package org.skyve.impl.web.faces;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.CookieManager;
import java.net.HttpCookie;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

import util.deployed.DeployedBrowser;
import util.deployed.DeployedCorrelation;
import util.deployed.DeployedDiagnostics;
import util.deployed.DeployedProbeClient;
import util.deployed.DeployedTestConfiguration;
import util.deployed.JsfPostbackSupport;

/** Exercises the deployed request, Mojarra state and UX/UI-selection lifecycle. */
@SuppressWarnings("java:S1192") // Repeated literals document observable lifecycle outcomes.
class ThemeResolutionLifecycleIT {
	private static final String CORRELATION_HEADER = "X-Skyve-Deployed-It-Correlation";
	private static final String CARRIED_PARAMETER = "_ua";
	private static final Pattern FORM_ACTION = Pattern.compile(
			"<form\\b[^>]*\\bid=([\"'])lifecycleForm\\1[^>]*\\baction=([\"'])(.*?)\\2",
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
	private static final Pattern EVENT = Pattern.compile("\\\"name\\\":\\\"([^\\\"]+)\\\"");

	private final DeployedTestConfiguration configuration = DeployedTestConfiguration.fromEnvironment();
	private final Path diagnostics = Path.of(
			"target/deployed-it/org.skyve.impl.web.faces.ThemeResolutionLifecycleIT/browser");

	@Test
	void deployedThemeLifecycleGate() throws Exception {
		Files.createDirectories(diagnostics);
		try (DeployedBrowser browser = new DeployedBrowser(configuration)) {
			browser.start();
			browser.login();
			List<HttpCookie> cookies = browser.authenticatedCookies();
			DeployedProbeClient probe = new DeployedProbeClient(configuration, cookies);
			HttpClient client = authenticatedClient(cookies);

			verifyEmulatedLifecycle(client, probe);
			verifyDirectLifecycle(client, probe);
			verifyChangedSelectionIsRejected(client, probe);
			verifyMarkerFreeViewIsRejectedAndFreshGetRecovers(client, probe);
		} catch (Throwable failure) {
			writeFailure(failure);
			throw failure;
		}
	}

	private void verifyEmulatedLifecycle(HttpClient client, DeployedProbeClient probe) throws Exception {
		preview(client, "tablet");
		Scenario scenario = start(probe);
		try {
			Page page = get(client,
					configuration.resolve("deployed-it/theme-lifecycle.xhtml"), scenario.correlation());
			assertFixture(page, "tablet", "tablet", true);
			assertTrue(parameterValues(page.action(), CARRIED_PARAMETER).isEmpty());

			HttpResponse<String> normal = post(client, page.action(), scenario.correlation(),
					JsfPostbackSupport.normalPostback("lifecycleForm", page.viewState(), "lifecycleForm:normal"), false);
			assertSuccess(normal);
			List<String> normalEvents = events(probe.read(scenario.correlation()));
			assertOrder(normalEvents, "MODEL_UPDATE", "ACTION_INVOKED");

			Page ajaxPage = get(client,
					configuration.resolve("deployed-it/theme-lifecycle.xhtml"), scenario.correlation());
			HttpResponse<String> ajax = post(client, ajaxPage.action(), scenario.correlation(),
					JsfPostbackSupport.ajaxPostback("lifecycleForm", ajaxPage.viewState(), "lifecycleForm:ajax",
							"lifecycleForm", "lifecycleForm:state"),
					true);
			assertSuccess(ajax);
			assertTrue(ajax.body().contains("partial-response"), ajax.body());
			List<String> ajaxEvents = events(probe.read(scenario.correlation()));
			assertEquals(eventCount(normalEvents, "MODEL_UPDATE") + 1,
					eventCount(ajaxEvents, "MODEL_UPDATE"));
			assertEquals(eventCount(normalEvents, "ACTION_INVOKED") + 1,
					eventCount(ajaxEvents, "ACTION_INVOKED"));
		} finally {
			clear(probe, scenario.correlation());
			preview(client, "");
		}
	}

	private void verifyDirectLifecycle(HttpClient client, DeployedProbeClient probe) throws Exception {
		preview(client, "");
		Scenario scenario = start(probe);
		try {
			Page page = get(client, configuration.resolve("deployed-it/theme-lifecycle.xhtml"), scenario.correlation());
			assertFixture(page, "phone", "other", false);
			assertTrue(parameterValues(page.action(), CARRIED_PARAMETER).isEmpty());
			HttpResponse<String> normal = post(client, page.action(), scenario.correlation(),
					JsfPostbackSupport.normalPostback("lifecycleForm", page.viewState(), "lifecycleForm:normal"), false);
			assertSuccess(normal);
			assertOrder(events(probe.read(scenario.correlation())), "MODEL_UPDATE", "ACTION_INVOKED");
		} finally {
			clear(probe, scenario.correlation());
		}
	}

	private void verifyChangedSelectionIsRejected(HttpClient client, DeployedProbeClient probe) throws Exception {
		preview(client, "");
		Scenario scenario = start(probe);
		try {
			Page page = get(client, configuration.resolve("deployed-it/theme-lifecycle.xhtml"), scenario.correlation());
			preview(client, "phone");
			HttpResponse<String> response = post(client, page.action(), scenario.correlation(),
					JsfPostbackSupport.normalPostback("lifecycleForm", page.viewState(), "lifecycleForm:normal"), false);
			assertSuccess(response);
			List<String> recorded = events(probe.read(scenario.correlation()));
			assertNoModelUpdateOrAction(recorded);
		} finally {
			clear(probe, scenario.correlation());
			preview(client, "");
		}
	}

	private void verifyMarkerFreeViewIsRejectedAndFreshGetRecovers(HttpClient client, DeployedProbeClient probe)
			throws Exception {
		preview(client, "");
		Scenario scenario = start(probe);
		try {
			Page markerFree = get(client,
					configuration.resolve("deployed-it/theme-lifecycle.xhtml?removeSelectionMarker=true"),
					scenario.correlation());
			List<String> initial = events(probe.read(scenario.correlation()));
			assertTrue(initial.contains("SELECTION_MARKER_REMOVED"), initial.toString());
			HttpResponse<String> rejected = post(client, markerFree.action(), scenario.correlation(),
					JsfPostbackSupport.normalPostback(
							"lifecycleForm", markerFree.viewState(), "lifecycleForm:normal"),
					false);
			assertSuccess(rejected);
			List<String> recorded = events(probe.read(scenario.correlation()));
			assertNoModelUpdateOrAction(recorded);

			Page recovered = get(client, configuration.resolve("deployed-it/theme-lifecycle.xhtml"), scenario.correlation());
			assertFixture(recovered, "phone", "other", false);
		} finally {
			clear(probe, scenario.correlation());
		}
	}

	private static void clear(DeployedProbeClient probe, DeployedCorrelation correlation) {
		try {
			probe.clear(correlation);
		} catch (@SuppressWarnings("unused") IOException e) {
			// Cleanup must not replace the lifecycle assertion that caused an error response.
		} catch (@SuppressWarnings("unused") InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}

	private static Scenario start(DeployedProbeClient probe) throws Exception {
		String correlationId = DeployedCorrelation.newCorrelationId();
		return new Scenario(probe.start(correlationId));
	}

	private Page get(HttpClient client, URI uri, DeployedCorrelation correlation) throws Exception {
		HttpRequest request = HttpRequest.newBuilder(uri)
				.timeout(configuration.getTimeout())
				.header(CORRELATION_HEADER, correlation.correlationId())
				.GET()
				.build();
		HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
		assertSuccess(response);
		Files.writeString(diagnostics.resolve("last-page.html"), redact(response.body()));
		String action = formAction(response.body());
		return new Page(response.body(), response.uri().resolve(action), JsfPostbackSupport.extractViewState(response.body()));
	}

	private HttpResponse<String> post(HttpClient client,
			URI action,
			DeployedCorrelation correlation,
			Map<String, String> values,
			boolean ajax) throws Exception {
		values.put("lifecycleForm:model", "updated");
		HttpRequest.Builder request = HttpRequest.newBuilder(action)
				.timeout(configuration.getTimeout())
				.header(CORRELATION_HEADER, correlation.correlationId())
				.header("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
				.POST(HttpRequest.BodyPublishers.ofString(JsfPostbackSupport.encode(values)));
		if (ajax) {
			request.header("Faces-Request", "partial/ajax");
		}
		HttpResponse<String> response = client.send(request.build(), HttpResponse.BodyHandlers.ofString());
		Files.writeString(diagnostics.resolve("last-postback.txt"), redact(response.body()));
		return response;
	}

	private void preview(HttpClient client, String type) throws Exception {
		URI command = withParameter(configuration.resolve("device.jsp"), CARRIED_PARAMETER, type);
		HttpResponse<Void> response = client.send(HttpRequest.newBuilder(command)
				.timeout(configuration.getTimeout())
				.GET()
				.build(), HttpResponse.BodyHandlers.discarding());
		assertSuccess(response);
	}

	private HttpClient authenticatedClient(List<HttpCookie> cookies) {
		CookieManager manager = new CookieManager();
		for (HttpCookie cookie : cookies) {
			manager.getCookieStore().add(configuration.getBaseUrl(), cookie);
		}
		return HttpClient.newBuilder()
				.cookieHandler(manager)
				.followRedirects(HttpClient.Redirect.ALWAYS)
				.connectTimeout(configuration.getTimeout())
				.build();
	}

	private static void assertFixture(Page page, String uxui, String userAgent, boolean emulated) {
		assertTrue(page.html().contains("data-uxui=\"" + uxui + "\""), page.html());
		assertTrue(page.html().contains("data-user-agent=\"" + userAgent + "\""), page.html());
		assertTrue(page.html().contains("data-emulated=\"" + emulated + "\""), page.html());
	}

	private static void assertSuccess(HttpResponse<?> response) {
		assertTrue((response.statusCode() >= 200) && (response.statusCode() < 300),
				() -> "Unexpected HTTP status " + response.statusCode() + " at " + response.uri());
	}

	private static String formAction(String html) {
		Matcher form = FORM_ACTION.matcher(html);
		if (!form.find()) {
			throw new IllegalArgumentException("Fixture response does not contain lifecycleForm");
		}
		return htmlText(form.group(3));
	}

	private static List<String> events(String json) {
		List<String> result = new ArrayList<>();
		Matcher event = EVENT.matcher(json);
		while (event.find()) {
			result.add(event.group(1));
		}
		return result;
	}

	private static void assertOrder(List<String> events, String... expected) {
		int index = -1;
		for (String event : expected) {
			int next = events.subList(index + 1, events.size()).indexOf(event);
			assertTrue(next >= 0, event + " was not found after index " + index + " in " + events);
			index += next + 1;
		}
	}

	private static void assertNoModelUpdateOrAction(List<String> events) {
		assertFalse(events.contains("MODEL_UPDATE"), events.toString());
		assertFalse(events.contains("ACTION_INVOKED"), events.toString());
	}

	private static long eventCount(List<String> events, String expected) {
		return events.stream().filter(expected::equals).count();
	}

	private static List<String> parameterValues(URI uri, String name) {
		List<String> result = new ArrayList<>();
		String query = uri.getRawQuery();
		if (query != null) {
			for (String pair : query.split("&")) {
				String[] parts = pair.split("=", 2);
				if (java.net.URLDecoder.decode(parts[0], StandardCharsets.UTF_8).equals(name)) {
					result.add(java.net.URLDecoder.decode((parts.length == 1) ? "" : parts[1], StandardCharsets.UTF_8));
				}
			}
		}
		return result;
	}

	private static URI withParameter(URI uri, String name, String value) {
		String encoded = URLEncoder.encode(name, StandardCharsets.UTF_8) + '=' +
				URLEncoder.encode(value, StandardCharsets.UTF_8);
		String query = uri.getRawQuery();
		String separator = ((query == null) || query.isEmpty()) ? "?" : "&";
		return URI.create(uri.toString() + separator + encoded);
	}

	private String redact(String value) {
		List<String> protectedValues = new ArrayList<>(2);
		protectedValues.add(configuration.getUsername());
		protectedValues.add(configuration.getPassword());
		return DeployedDiagnostics.redact(value, protectedValues);
	}

	private void writeFailure(Throwable failure) throws IOException {
		Files.createDirectories(diagnostics);
		Files.writeString(diagnostics.resolve("failure.txt"), redact(failure.toString()));
	}

	private static String htmlText(String value) {
		return value.replace("&quot;", "\"")
				.replace("&#39;", "'")
				.replace("&lt;", "<")
				.replace("&gt;", ">")
				.replace("&amp;", "&");
	}

	private record Page(String html, URI action, String viewState) {
		// Immutable page state.
	}

	private record Scenario(DeployedCorrelation correlation) {
		// Correlation-scoped scenario.
	}
}
