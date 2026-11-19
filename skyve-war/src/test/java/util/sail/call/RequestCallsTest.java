package util.sail.call;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static util.sail.call.AbstractSailCallTest.context;
import static util.sail.call.AbstractSailCallTest.run;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;

import util.sail.browser.FakeBrowserControl;
import util.sail.browser.LoggedRequest;
import util.sail.browser.RequestRule;
import util.sail.browser.RequestRule.ErrorReason;

/**
 * Tests the network, request rule and request check calls against a fake browser control: what each asks of the
 * browser, its defaults, its parameter checks, and what it returns.
 */
@SuppressWarnings("static-method")
class RequestCallsTest {
	private static final LoggedRequest PAGE_COPY = new LoggedRequest("GET", "http://h/sw/a", Integer.valueOf(200), null, true);
	private static final LoggedRequest WORKER = new LoggedRequest("GET", "http://h/sw/a", Integer.valueOf(200), null, false);
	private static final LoggedRequest FAILED = new LoggedRequest("POST", "http://h/save", null, "net::ERR_FAILED", false);
	private static final LoggedRequest PLAIN = new LoggedRequest("GET", "http://h/plain?x=\"1\"", null, null, false);

	private static String message(Class<? extends Throwable> type, org.junit.jupiter.api.function.Executable executable) {
		return assertThrows(type, executable).getMessage();
	}

	// ---- network

	@Test
	void theNetworkCallsSetTheBrowsersNetwork() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(browser);
		run(new Offline(), context);
		run(new Online(), context);
		run(new Throttle(), context, "latencyMillis", "300");
		run(new Throttle(), context, "latencyMillis", "0", "downKbps", "800", "upKbps", "-1");
		assertAll(() -> assertEquals(List.of("offline", "online", "throttle PT0.3S -1 -1", "throttle PT0S 100000 -1"), browser.calls()),
					() -> assertEquals("Throttle needs the parameter latencyMillis", message(IllegalArgumentException.class, () -> run(new Throttle(), context))),
					() -> assertEquals("Throttle's downKbps must be -1 or more, not -2",
										message(IllegalArgumentException.class, () -> run(new Throttle(), context, "latencyMillis", "1", "downKbps", "-2"))));
	}

	// ---- request rules

	@Test
	void theRuleCallsAddTheirRules() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(browser);
		run(new FailRequests(), context, "url", "*/save*");
		run(new FailRequests(), context, "url", "*/save*", "reason", "TimedOut", "method", " POST ", "times", "2");
		run(new HoldRequests(), context, "url", "*/slow*", "times", "1");
		run(new RespondToRequests(), context, "url", "*/api*", "status", "503");
		run(new RespondToRequests(), context, "url", "*/api*", "status", "409", "body", "no", "contentType", "text/plain", "atResponse", "true", "method", "PUT");
		run(new LoseResponses(), context, "url", "*/save*");
		run(new LoseResponses(), context, "url", "*/save*", "reason", "TIMED_OUT", "times", "1");
		run(new RewriteRequestHeaders(), context, "url", "*", "set", "X-Client=1; X-Other = two ;", "remove", "X-Csrf, Cookie");
		assertEquals(List.of(RequestRule.fail("*/save*", ErrorReason.INTERNET_DISCONNECTED),
								RequestRule.fail("*/save*", ErrorReason.TIMED_OUT).method("POST").times(2),
								RequestRule.hold("*/slow*").times(1),
								RequestRule.fulfil("*/api*", 503, Map.of("Content-Type", "application/json"), ""),
								RequestRule.fulfil("*/api*", 409, Map.of("Content-Type", "text/plain"), "no").atResponse().method("PUT"),
								RequestRule.loseResponse("*/save*", ErrorReason.CONNECTION_RESET),
								RequestRule.loseResponse("*/save*", ErrorReason.TIMED_OUT).times(1),
								RequestRule.headers("*", Map.of("X-Client", "1", "X-Other", "two"), Set.of("X-Csrf", "Cookie"))),
						browser.rules());
	}

	@Test
	void theRuleCallsCheckTheirParameters() {
		AbstractSailCall.Context context = context(FakeBrowserControl.supportingAll());
		assertAll(() -> assertEquals("FailRequests needs the parameter url", message(IllegalArgumentException.class, () -> run(new FailRequests(), context))),
					() -> assertTrue(message(IllegalArgumentException.class, () -> run(new FailRequests(), context, "url", "*", "reason", "Busy"))
										.startsWith("FailRequests's reason must be one of [FAILED, ABORTED"), "FailRequests reason"),
					() -> assertEquals("HoldRequests's times must be 0 or more, not -1",
										message(IllegalArgumentException.class, () -> run(new HoldRequests(), context, "url", "*", "times", "-1"))),
					() -> assertEquals("RespondToRequests's status must be 100 or more, not 99",
										message(IllegalArgumentException.class, () -> run(new RespondToRequests(), context, "url", "*", "status", "99"))),
					() -> assertEquals("RespondToRequests's status must be 599 or less, not 600",
										message(IllegalArgumentException.class, () -> run(new RespondToRequests(), context, "url", "*", "status", "600"))),
					() -> assertEquals("RespondToRequests needs the parameter status",
										message(IllegalArgumentException.class, () -> run(new RespondToRequests(), context, "url", "*"))),
					() -> assertEquals("RewriteRequestHeaders's set must be Name=value pairs separated by ;, not A=1;B",
										message(IllegalArgumentException.class, () -> run(new RewriteRequestHeaders(), context, "url", "*", "set", "A=1;B"))),
					() -> assertEquals("RewriteRequestHeaders's set must be Name=value pairs separated by ;, not =1",
										message(IllegalArgumentException.class, () -> run(new RewriteRequestHeaders(), context, "url", "*", "set", "=1"))),
					() -> assertEquals("RewriteRequestHeaders needs headers to set or remove",
										message(IllegalArgumentException.class, () -> run(new RewriteRequestHeaders(), context, "url", "*"))),
					() -> assertEquals("RewriteRequestHeaders's header names must be HTTP tokens (letters, digits and !#$%&'*+-.^_`|~), not X Bad",
										message(IllegalArgumentException.class, () -> run(new RewriteRequestHeaders(), context, "url", "*", "set", "X Bad=1")),
										"a name to set with a space"),
					() -> assertEquals("RewriteRequestHeaders's header names must be HTTP tokens (letters, digits and !#$%&'*+-.^_`|~), not X:Bad",
										message(IllegalArgumentException.class, () -> run(new RewriteRequestHeaders(), context, "url", "*", "remove", "Ok, X:Bad")),
										"a name to remove with a colon"));
	}

	@Test
	void heldRequestsAreAwaitedReleasedFailedAndCleared() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withHeld(2);
		AbstractSailCall.Context context = context(browser);
		String held = run(new AwaitHeldRequests(), context);
		String two = run(new AwaitHeldRequests(), context, "count", "2", "timeoutMillis", "500");
		run(new ReleaseRequests(), context);
		browser.withHeld(1);
		run(new ReleaseRequests(), context, "fail", "ConnectionReset");
		run(new ClearRequestRules(), context);
		AwaitHeldRequests awaitNone = new AwaitHeldRequests();
		AssertionError none = assertThrows(AssertionError.class, () -> run(awaitNone, context));
		assertAll(() -> assertEquals("2", held, "the number held"),
					() -> assertEquals("2", two),
					() -> assertEquals(List.of("awaitHeld 1 PT7S", "heldCount", "awaitHeld 2 PT0.5S", "heldCount", "releaseHeld",
												"failHeld CONNECTION_RESET", "clearRules", "awaitHeld 1 PT7S"),
										browser.calls(), "the leg's timeout by default"),
					() -> assertEquals("Expected 1 held requests within PT7S, but 0 were held", none.getMessage()),
					() -> assertEquals("AwaitHeldRequests's count must be 1 or more, not 0",
										message(IllegalArgumentException.class, () -> run(new AwaitHeldRequests(), context, "count", "0"))));
	}

	// ---- request checks

	@Test
	void checkRequestsCountsTheMatchingRequestsOnce() {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withRequests(PAGE_COPY, WORKER, FAILED, PLAIN);
		AbstractSailCall.Context context = context(browser);
		assertAll(() -> assertEquals("1", run(new CheckRequests(), context, "matching", "GET .*/sw/a"), "the service worker's copy left out"),
					() -> assertEquals("2", run(new CheckRequests(), context, "matching", "GET .*/sw/a", "ignoreServiceWorkerCopies", "false",
													"exactly", "2")),
					() -> assertEquals("1", run(new CheckRequests(), context, "matching", "^POST ", "failed", "true")),
					() -> assertEquals("2", run(new CheckRequests(), context, "matching", "http", "failed", "false", "atLeast", "2", "atMost", "2")),
					() -> assertEquals("0", run(new CheckRequests(), context, "matching", "nothing", "exactly", "0")),
					() -> assertEquals("0", run(new CheckRequests(), context, "matching", "nothing", "atMost", "0")));
	}

	@Test
	void checkRequestsFailsListingWhatMatchedAndCanClear() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withRequests(WORKER, FAILED);
		AbstractSailCall.Context context = context(browser);
		CheckRequests check = new CheckRequests();
		AssertionError none = assertThrows(AssertionError.class, () -> run(check, context, "matching", "/nothing"));
		AssertionError many = assertThrows(AssertionError.class, () -> run(check, context, "matching", "http", "atMost", "1", "clear", "true"));
		String cleared = run(new CheckRequests(), context, "matching", "http", "exactly", "0");
		assertAll(() -> assertEquals("Expected requests matching //nothing/ (at least 1), but 0 matched: [] (the log holds 2 requests; the last: "
										+ List.of(WORKER, FAILED) + ')',
										none.getMessage()),
					() -> assertTrue(many.getMessage().startsWith("Expected requests matching /http/ (at most 1), but 2 matched: " + List.of(WORKER, FAILED)),
										many.getMessage()),
					() -> assertEquals("0", cleared, "cleared even though the check failed"),
					() -> assertTrue(message(IllegalArgumentException.class, () -> run(new CheckRequests(), context, "matching", "(open"))
										.startsWith("CheckRequests's matching must be a regular expression, not (open: "), "a bad expression"),
					() -> assertEquals("CheckRequests needs the parameter matching",
										message(IllegalArgumentException.class, () -> run(new CheckRequests(), context))));
	}

	/**
	 * A fake browser whose page sends a request only after the log has been read a few times, as a page sends one a
	 * moment after the step before.
	 */
	private static final class LatePage extends FakeBrowserControl {
		private int reads;

		LatePage() {
			super("chrome", util.sail.browser.Capability.values());
		}

		@Override
		public List<LoggedRequest> requests() {
			List<LoggedRequest> result = new ArrayList<>(super.requests());
			if (++reads >= 3) {
				result.add(PLAIN);
			}
			return result;
		}
	}

	/** waitMillis keeps looking for the count wanted, so a request the page sends a little later is found. */
	@Test
	void checkRequestsWaitsForARequestTheStepBeforeStarted() throws Exception {
		LatePage waited = new LatePage();
		long start = System.nanoTime();
		String found = run(new CheckRequests(), context(waited), "matching", "/plain", "waitMillis", "5000");
		Duration took = Duration.ofNanos(System.nanoTime() - start);
		CheckRequests check = new CheckRequests();
		AbstractSailCall.Context late = context(new LatePage());
		AssertionError once = assertThrows(AssertionError.class, () -> run(check, late, "matching", "/plain"));
		assertAll(() -> assertEquals("1", found),
					() -> assertTrue(took.compareTo(Duration.ofSeconds(4)) < 0, "it stops waiting once found, after " + took),
					() -> assertTrue(once.getMessage().contains("but 0 matched"), "without waitMillis it looks once: " + once.getMessage()));
	}

	@Test
	void checkRequestsStopsWaitingAtTheTime() {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		CheckRequests check = new CheckRequests();
		AbstractSailCall.Context context = context(browser);
		AssertionError e = assertThrows(AssertionError.class, () -> run(check, context, "matching", "/plain", "waitMillis", "300"));
		long reads = browser.calls().stream().filter("requests"::equals).count();
		assertAll(() -> assertTrue(e.getMessage().contains("but 0 matched"), e.getMessage()),
					() -> assertTrue(reads > 1, "the log was read again while waiting: " + reads));
	}

	/**
	 * A check of a log that began in its own step fails, as the log holds nothing from the steps before and would pass
	 * a check that nothing was sent; it says to start watching with an earlier call. A log that began before the step is
	 * checked.
	 */
	@Test
	void checkRequestsFailsWhenTheLogBeganInItsStep() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withWatchingSince(Instant.now().minusSeconds(1));
		AbstractSailCall.Context before = context(browser);
		String counted = run(new CheckRequests(), before, "matching", "nothing", "exactly", "0");
		AbstractSailCall.Context step = context(browser);
		browser.withWatchingSince(Instant.now()); // the control began watching as this step asked for it
		CheckRequests check = new CheckRequests();
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> run(check, step, "matching", "nothing", "exactly", "0"));
		assertAll(() -> assertEquals("0", counted, "a log that began before the step"),
					() -> assertEquals("CheckRequests reads the request log, which only began in this step, as the browser's control began watching then:"
										+ " it holds nothing from the steps before. Put a call before the steps to watch, such as RequestLog with"
										+ " clear=true, so watching begins there",
										e.getMessage()));
	}

	/**
	 * The request log call follows the same rule: in the step where the log began it fails, unless it is told to clear
	 * the log, which starts it there and gives an empty log.
	 */
	@Test
	void theRequestLogFailsWhenItBeganInItsStepUnlessItStartsIt() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withRequests(PLAIN);
		AbstractSailCall.Context step = context(browser);
		browser.withWatchingSince(Instant.now()); // the control began watching as this step asked for it
		RequestLog read = new RequestLog();
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> run(read, step, "matching", "plain"));
		String started = run(new RequestLog(), step, "clear", "true");
		assertAll(() -> assertEquals("RequestLog reads the request log, which only began in this step, as the browser's control began watching then:"
										+ " it holds nothing from the steps before. Put a call before the steps to watch, such as RequestLog with"
										+ " clear=true, so watching begins there",
										e.getMessage()),
					() -> assertEquals("[]", started, "started"),
					() -> assertEquals(List.of(), browser.requests(), "the log was cleared as it started"));
	}

	@Test
	void theRequestLogIsJsonOfTheMatchingRequests() throws Exception {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll().withRequests(PAGE_COPY, FAILED, PLAIN);
		AbstractSailCall.Context context = context(browser);
		String all = run(new RequestLog(), context);
		String posts = run(new RequestLog(), context, "matching", "^POST", "clear", "true");
		String none = run(new RequestLog(), context);
		assertAll(() -> assertEquals("[{\"method\":\"GET\",\"url\":\"http://h/sw/a\",\"status\":200,\"failure\":null,\"fromServiceWorker\":true},"
										+ "{\"method\":\"POST\",\"url\":\"http://h/save\",\"status\":null,\"failure\":\"net::ERR_FAILED\",\"fromServiceWorker\":false},"
										+ "{\"method\":\"GET\",\"url\":\"http://h/plain?x=\\\"1\\\"\",\"status\":null,\"failure\":null,\"fromServiceWorker\":false}]",
										all),
					() -> assertEquals("[{\"method\":\"POST\",\"url\":\"http://h/save\",\"status\":null,\"failure\":\"net::ERR_FAILED\",\"fromServiceWorker\":false}]",
										posts),
					() -> assertEquals("[]", none, "cleared"));
	}
}
