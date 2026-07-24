package util.deployed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.time.Duration;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

@SuppressWarnings({ "static-method", "java:S1192" }) // Repeated literals are deliberate deployed-transport fixtures.
class DeployedTestSupportTest {
	@Test
	void configurationUsesSafeLocalDefaultsWithoutCredentials() {
		DeployedTestConfiguration configuration = DeployedTestConfiguration.from(Map.of());

		assertEquals(URI.create("http://127.0.0.1:8080/skyve/"), configuration.getBaseUrl());
		assertEquals(Duration.ofSeconds(30), configuration.getTimeout());
		assertTrue(configuration.isHeadless());
		assertEquals(null, configuration.getUsername());
		assertEquals(null, configuration.getPassword());
	}

	@Test
	void configurationNormalisesOverridesAndRejectsUnsafeBaseUrls() {
		DeployedTestConfiguration configuration = DeployedTestConfiguration.from(Map.of(
				"SKYVE_DEPLOYED_IT_BASE_URL", "https://example.test/app",
				"SKYVE_DEPLOYED_IT_TIMEOUT_SECONDS", "45",
				"SKYVE_DEPLOYED_IT_HEADLESS", "false"));

		assertEquals(URI.create("https://example.test/app/"), configuration.getBaseUrl());
		assertEquals(Duration.ofSeconds(45), configuration.getTimeout());
		assertFalse(configuration.isHeadless());
		assertThrows(IllegalArgumentException.class,
				() -> DeployedTestConfiguration.from(Map.of("SKYVE_DEPLOYED_IT_BASE_URL", "file:/tmp/app")));
	}

	@Test
	void extractsJakartaViewStateRegardlessOfAttributeOrderAndEscaping() {
		String html = "<form><input value='server&amp;state' type='hidden' name='jakarta.faces.ViewState'></form>";

		assertEquals("server&state", JsfPostbackSupport.extractViewState(html));
		assertThrows(IllegalArgumentException.class, () -> JsfPostbackSupport.extractViewState("<form></form>"));
	}

	@Test
	void constructsNormalAndPrimeFacesAjaxPostbacks() {
		Map<String, String> normal = JsfPostbackSupport.normalPostback("form", "state+1", "form:save");
		Map<String, String> ajax = JsfPostbackSupport.ajaxPostback("form", "state+1", "form:save", "@this", "form:messages");

		assertEquals("state+1", normal.get("jakarta.faces.ViewState"));
		assertEquals("form:save", normal.get("jakarta.faces.source"));
		assertEquals("true", ajax.get("jakarta.faces.partial.ajax"));
		assertEquals("@this", ajax.get("jakarta.faces.partial.execute"));
		assertTrue(JsfPostbackSupport.encode(normal).contains("state%2B1"));
	}

	@Test
	void correlationIdsAndHeadersUseOnlyTransportVocabulary() {
		String correlationId = DeployedCorrelation.newCorrelationId();
		DeployedCorrelation correlation = new DeployedCorrelation(correlationId, "server-token");

		assertTrue(correlationId.matches("dit-[0-9a-f]{32}"));
		assertEquals(correlationId, correlation.headers().get("X-Skyve-Deployed-It-Correlation"));
		assertEquals("server-token", correlation.headers().get("X-Skyve-Deployed-It-Token"));
	}

	@Test
	@SuppressWarnings("java:S2068") // Deliberate fake secrets verify diagnostic redaction.
	void diagnosticsRedactAssignmentsProtectedValuesAndUrlQueries() {
		String diagnostic = "password=hunter2 token:abc cookie=session42 visible=yes";
		String redacted = DeployedDiagnostics.redact(diagnostic, List.of("session42", "extra-secret"));

		assertFalse(redacted.contains("hunter2"));
		assertFalse(redacted.contains("abc"));
		assertFalse(redacted.contains("session42"));
		assertTrue(redacted.contains("visible=yes"));
		assertEquals("https://example.test:8443/skyve/path",
				DeployedDiagnostics.endpointShape(URI.create("https://user:secret@example.test:8443/skyve/path?token=x")));
	}
}
