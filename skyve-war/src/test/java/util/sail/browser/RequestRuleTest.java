package util.sail.browser;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import util.sail.browser.RequestRule.ErrorReason;
import util.sail.browser.RequestRule.Fail;
import util.sail.browser.RequestRule.Fulfil;
import util.sail.browser.RequestRule.Headers;
import util.sail.browser.RequestRule.Stage;

/**
 * Tests {@link RequestRule}, which needs no browser: URL globs, which requests a rule matches, its checks, and its
 * actions keeping what they were given. Ported from basix-cloud's {@code FetchRuleTest}.
 */
@SuppressWarnings("static-method")
class RequestRuleTest {
	@Test
	void urlGlobsMatchTheWholeUrl() {
		record Case(String pattern, String url, boolean matches) {
		}
		Stream<Case> cases = Stream.of(new Case("*", "http://h/a", true),
										new Case("*/api/sync*", "http://h/app/api/sync/3?x=1", true),
										new Case("*/api/sync*", "http://h/app/api/media", false),
										new Case("http://h/a", "http://h/a", true),
										new Case("http://h/a", "http://h/ab", false),
										new Case("http://h/a?", "http://h/ab", true),
										new Case("http://h/a?", "http://h/a", false),
										new Case("*a*b*c", "xxaxxbxxc", true),
										new Case("*a*b*c", "xxaxxcxxb", false),
										new Case("**", "", true),
										new Case("*\\*", "file*", true),
										new Case("*\\*", "file", false),
										new Case("a\\?", "a?", true),
										new Case("a\\?", "ab", false),
										new Case("ab\\", "ab\\", true));
		assertAll(cases.map(c -> (Executable) () -> assertEquals(Boolean.valueOf(c.matches()),
																	Boolean.valueOf(RequestRule.globMatches(c.pattern(), c.url())),
																	c.pattern() + " against " + c.url())));
	}

	@Test
	void aRuleMatchesItsStageMethodAndUrl() {
		RequestRule rule = RequestRule.fail("*/save*", ErrorReason.TIMED_OUT).method("post");
		assertAll(() -> assertTrue(rule.matches("http://h/save/1", "POST", Stage.REQUEST), "POST, request stage"),
					() -> assertFalse(rule.matches("http://h/save/1", "GET", Stage.REQUEST), "another method"),
					() -> assertFalse(rule.matches("http://h/load/1", "POST", Stage.REQUEST), "another URL"),
					() -> assertFalse(rule.matches("http://h/save/1", "POST", Stage.RESPONSE), "another stage"),
					() -> assertTrue(rule.atResponse().matches("http://h/save/1", "POST", Stage.RESPONSE), "at the response"),
					() -> assertTrue(RequestRule.hold("*").matches("http://h/x", "DELETE", Stage.REQUEST), "no method: any"));
	}

	@Test
	void narrowingKeepsTheRestOfTheRule() {
		RequestRule rule = RequestRule.fulfil("*/a*", 503, Map.of("Content-Type", "text/plain"), "busy").method("GET").times(2).atResponse();
		assertAll(() -> assertEquals("*/a*", rule.urlPattern()),
					() -> assertEquals("GET", rule.method()),
					() -> assertEquals(Stage.RESPONSE, rule.stage()),
					() -> assertEquals(2, rule.times()),
					() -> assertEquals(new Fulfil(503, Map.of("Content-Type", "text/plain"), "busy"), rule.action()),
					() -> assertEquals(0, RequestRule.hold("*").times(), "every time by default"));
	}

	@Test
	void eachFactoryMakesItsActionAtItsStage() {
		RequestRule lose = RequestRule.loseResponse("*/save*", ErrorReason.CONNECTION_RESET);
		RequestRule headers = RequestRule.headers("*", Map.of("X-Client-Version", "0"), Set.of("Cookie"));
		assertAll(() -> assertEquals(new Fail(ErrorReason.CONNECTION_RESET), lose.action(), "lose a response: fail"),
					() -> assertEquals(Stage.RESPONSE, lose.stage(), "lose a response: once the server has answered"),
					() -> assertEquals(Stage.REQUEST, RequestRule.fail("*", ErrorReason.FAILED).stage(), "fail: before it is sent"),
					() -> assertEquals(new Headers(Map.of("X-Client-Version", "0"), Set.of("Cookie")), headers.action(), "headers"),
					() -> assertEquals(Stage.REQUEST, headers.stage(), "headers: the request's"),
					() -> assertTrue(RequestRule.hold("*").action() instanceof RequestRule.Hold, "hold"));
	}

	@Test
	void badRulesAreRefused() {
		RequestRule hold = RequestRule.hold("*");
		assertAll(() -> assertThrows(IllegalArgumentException.class, () -> RequestRule.hold(" "), "a blank pattern"),
					() -> assertThrows(IllegalArgumentException.class, () -> hold.times(-1), "negative times"),
					() -> assertThrows(IllegalArgumentException.class, hold::atResponse, "holding a response"));
	}

	@Test
	void actionsCopyWhatTheyAreGiven() {
		Map<String, String> headers = new LinkedHashMap<>(Map.of("A", "1"));
		Fulfil fulfil = new Fulfil(200, headers, "");
		Headers change = new Headers(headers, new HashSet<>(Set.of("B")));
		headers.put("C", "3");
		Set<String> removed = change.remove();
		assertAll(() -> assertEquals(Map.of("A", "1"), fulfil.headers(), "fulfil"),
					() -> assertEquals(Map.of("A", "1"), change.set(), "headers"),
					() -> assertThrows(UnsupportedOperationException.class, () -> removed.add("D"), "unmodifiable"));
	}
}
