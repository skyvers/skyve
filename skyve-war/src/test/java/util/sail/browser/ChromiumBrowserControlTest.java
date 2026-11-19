package util.sail.browser;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import util.sail.browser.RequestRule.ErrorReason;
import util.sail.browser.RequestRule.Headers;
import util.sail.browser.RequestRule.Stage;

/**
 * Tests the parts of {@link ChromiumBrowserControl} that need no browser: the DevTools names of stages, errors and
 * dialogs, how headers are changed, and the command each rule answers a paused request with. Ported from basix-cloud's
 * {@code FetchRuleTest}; {@link RequestRuleTest} has the rest. {@code ChromiumBrowserControlIT} proves them in Chrome.
 */
@SuppressWarnings("static-method")
class ChromiumBrowserControlTest {
	@Test
	void stagesAndErrorsAreSentByTheirDevToolsNames() {
		assertAll(() -> assertEquals("InternetDisconnected", ChromiumBrowserControl.devToolsName(ErrorReason.INTERNET_DISCONNECTED)),
					() -> assertEquals("TimedOut", ChromiumBrowserControl.devToolsName(ErrorReason.TIMED_OUT)),
					() -> assertEquals("BlockedByResponse", ChromiumBrowserControl.devToolsName(ErrorReason.BLOCKED_BY_RESPONSE)),
					() -> assertEquals("Request", ChromiumBrowserControl.devToolsName(Stage.REQUEST)),
					() -> assertEquals("Response", ChromiumBrowserControl.devToolsName(Stage.RESPONSE)));
	}

	/** Every error's DevTools name is its constant in camel case, as {@code Network.ErrorReason} spells them. */
	@Test
	void everyErrorHasItsDevToolsName() {
		assertAll(Stream.of(ErrorReason.values()).map(reason -> (Executable) () -> {
			StringBuilder expected = new StringBuilder();
			for (String word : reason.name().split("_")) {
				expected.append(word.charAt(0)).append(word.substring(1).toLowerCase());
			}
			assertEquals(expected.toString(), ChromiumBrowserControl.devToolsName(reason), reason.name());
		}));
	}

	@Test
	void dialogTypesAreReadFromTheirDevToolsNames() {
		assertAll(Stream.of(Dialog.Type.values())
						.map(type -> (Executable) () -> assertEquals(type,
																		ChromiumBrowserControl.dialogType(type.name().toLowerCase()),
																		type.name())));
	}

	@Test
	void headersAreRemovedAndSetInAnyCase() {
		Map<String, String> original = new LinkedHashMap<>();
		original.put("Accept", "*/*");
		original.put("X-Client-Version", "1");
		original.put("Cookie", "a=b");
		Map<String, String> merged = ChromiumBrowserControl.mergeHeaders(original, new Headers(Map.of("x-client-version", "0"), Set.of("COOKIE", "Not-There")));
		assertEquals(List.of("Accept=*/*", "x-client-version=0"), merged.entrySet().stream().map(e -> e.getKey() + '=' + e.getValue()).toList());
	}

	@Test
	void eachRuleAnswersAPausedRequestWithItsCommand() {
		Map<String, Object> request = Map.of("requestId", "r1",
												"request", Map.of("url", "http://h/a", "method", "GET", "headers", Map.of("Accept", "*/*", "X-Old", "1")));
		Map<String, Object> response = Map.of("requestId", "r1",
												"request", Map.of("url", "http://h/a", "method", "GET"),
												"responseStatusCode", Integer.valueOf(200),
												"responseHeaders", List.of(Map.of("name", "X-Server", "value", "yes")));
		Map<String, Object> failedResponse = Map.of("requestId", "r1", "responseErrorReason", "Failed");
		RequestRule headers = RequestRule.headers("*", Map.of("X-New", "2"), Set.of("x-old", "x-server"));
		String busy = Base64.getEncoder().encodeToString("busy".getBytes(StandardCharsets.UTF_8));
		assertAll(() -> assertEquals(command("Fetch.continueRequest", Map.of("requestId", "r1")),
										ChromiumBrowserControl.answer("r1", request, null, Stage.REQUEST), "no rule, request"),
					() -> assertEquals(command("Fetch.continueResponse", Map.of("requestId", "r1")),
										ChromiumBrowserControl.answer("r1", response, null, Stage.RESPONSE), "no rule, response"),
					() -> assertEquals(command("Fetch.continueRequest", Map.of("requestId", "r1")),
										ChromiumBrowserControl.answer("r1", failedResponse, null, Stage.RESPONSE), "no rule, failed response"),
					() -> assertEquals(command("Fetch.failRequest", Map.of("requestId", "r1", "errorReason", "ConnectionReset")),
										ChromiumBrowserControl.answer("r1", response, RequestRule.loseResponse("*", ErrorReason.CONNECTION_RESET), Stage.RESPONSE),
										"lose the response"),
					() -> assertEquals(command("Fetch.fulfillRequest", Map.of("requestId", "r1",
																				"responseCode", Integer.valueOf(503),
																				"responseHeaders", List.of(Map.of("name", "Content-Type", "value", "text/plain")),
																				"body", busy)),
										ChromiumBrowserControl.answer("r1", request, RequestRule.fulfil("*", 503, Map.of("Content-Type", "text/plain"), "busy"), Stage.REQUEST),
										"fulfil"),
					() -> assertEquals(command("Fetch.continueRequest", Map.of("requestId", "r1",
																				"headers", List.of(Map.of("name", "Accept", "value", "*/*"),
																									Map.of("name", "X-New", "value", "2")))),
										ChromiumBrowserControl.answer("r1", request, headers, Stage.REQUEST), "the request's headers"),
					() -> assertEquals(command("Fetch.continueResponse", Map.of("requestId", "r1",
																				"responseCode", Integer.valueOf(200),
																				"responseHeaders", List.of(Map.of("name", "X-New", "value", "2")))),
										ChromiumBrowserControl.answer("r1", response, headers.atResponse(), Stage.RESPONSE), "the response's headers"),
					() -> assertEquals(command("Fetch.continueRequest", Map.of("requestId", "r1")),
										ChromiumBrowserControl.answer("r1", failedResponse, headers.atResponse(), Stage.RESPONSE),
										"the headers of a response that failed: carries on"));
	}

	private static Map<String, Object> command(String method, Map<String, Object> params) {
		return Map.of("method", method, "params", params);
	}
}
