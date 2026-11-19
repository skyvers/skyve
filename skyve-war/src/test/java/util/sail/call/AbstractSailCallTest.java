package util.sail.call;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.opentest4j.TestAbortedException;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import util.sail.PrimeFacesSelenide;
import util.sail.browser.FakeBrowserControl;
import util.sail.browser.SailBrowserControl;
import util.sail.browser.SailCapabilityUnavailableException;

/**
 * Tests what every standard call shares through its base: a parameter it doesn't take is refused, naming the call and
 * the names it takes; it needs a browser; and in a browser that lacks what it needs, it skips the test, naming itself,
 * the capability and the browser, before doing anything. Also gives the other call tests their leg: a fake browser
 * control, a mock driver and a mock Selenide.
 */
@SuppressWarnings("static-method")
class AbstractSailCallTest {
	/** The leg's timeout in the call tests. */
	static final Duration TIMEOUT = Duration.ofSeconds(7);
	/** The leg's base URL in the call tests. */
	static final String BASE_URL = "http://h/app/";

	/**
	 * Makes a leg whose browser is a fake control, with a mock driver that runs scripts.
	 *
	 * @param control	the browser's control
	 * @return the leg
	 */
	static @Nonnull AbstractSailCall.Context context(@Nonnull SailBrowserControl control) {
		return context(mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class)), control);
	}

	/**
	 * Makes a leg with a driver and a control, a Selenide with {@link #TIMEOUT} and {@link #BASE_URL}, and the run
	 * token {@code run1}.
	 *
	 * @param driver	the browser's driver
	 * @param control	the browser's control
	 * @return the leg
	 */
	static @Nonnull AbstractSailCall.Context context(@Nonnull WebDriver driver, @Nonnull SailBrowserControl control) {
		return context(() -> driver, mockSelenide(), d -> control);
	}

	/**
	 * Makes a leg from its parts, with the run token {@code run1}.
	 *
	 * @param driver	gives the driver
	 * @param selenide	the leg's Selenide
	 * @param controls	gives a driver's control
	 * @return the leg
	 */
	static @Nonnull AbstractSailCall.Context context(@Nonnull Supplier<WebDriver> driver,
														@Nonnull PrimeFacesSelenide selenide,
														@Nonnull Function<WebDriver, SailBrowserControl> controls) {
		return new AbstractSailCall.Context(driver, selenide, controls, () -> "run1");
	}

	/**
	 * Makes a Selenide that gives {@link #TIMEOUT} and {@link #BASE_URL}.
	 *
	 * @return the mock
	 */
	static @Nonnull PrimeFacesSelenide mockSelenide() {
		PrimeFacesSelenide result = mock(PrimeFacesSelenide.class);
		when(result.timeout()).thenReturn(TIMEOUT);
		when(result.getBaseUrl()).thenReturn(BASE_URL);
		return result;
	}

	/**
	 * Runs a call with parameters given as name, value, name, value...
	 *
	 * @param call	the call
	 * @param context	the leg
	 * @param namesAndValues	the parameters
	 * @return what the call returns
	 * @throws Exception	what the call throws
	 */
	static String run(@Nonnull AbstractSailCall call, @Nonnull AbstractSailCall.Context context, @Nonnull String... namesAndValues) throws Exception {
		return call.call(parameters(namesAndValues), context);
	}

	/**
	 * Makes a call's parameters from name, value, name, value...
	 *
	 * @param namesAndValues	the parameters
	 * @return the map, which the call may change
	 */
	static @Nonnull Map<String, String> parameters(@Nonnull String... namesAndValues) {
		Map<String, String> result = new HashMap<>();
		for (int i = 0; i < namesAndValues.length; i += 2) {
			result.put(namesAndValues[i], namesAndValues[i + 1]);
		}
		return result;
	}

	/** A call, the parameters it takes, and valid parameters for it. */
	private record Case(@Nonnull AbstractSailCall call, @Nonnull String takes, @Nonnull String needs, @Nonnull String... valid) {
		String name() {
			return call.getClass().getSimpleName();
		}
	}

	/** Every standard call: what it takes, what it needs from the browser, and valid parameters. */
	private static List<Case> cases() {
		return List.of(new Case(new Offline(), "none", "NETWORK_CONDITIONS"),
						new Case(new Online(), "none", "NETWORK_CONDITIONS"),
						new Case(new Throttle(), "[downKbps, latencyMillis, upKbps]", "NETWORK_CONDITIONS", "latencyMillis", "100"),
						new Case(new FailRequests(), "[method, reason, times, url]", "REQUEST_INTERCEPTION", "url", "*/x"),
						new Case(new HoldRequests(), "[method, times, url]", "REQUEST_INTERCEPTION", "url", "*/x"),
						new Case(new AwaitHeldRequests(), "[count, timeoutMillis]", "REQUEST_INTERCEPTION"),
						new Case(new ReleaseRequests(), "[fail]", "REQUEST_INTERCEPTION"),
						new Case(new RespondToRequests(), "[atResponse, body, contentType, method, status, times, url]", "REQUEST_INTERCEPTION",
									"url", "*/x", "status", "503"),
						new Case(new LoseResponses(), "[method, reason, times, url]", "REQUEST_INTERCEPTION", "url", "*/x"),
						new Case(new RewriteRequestHeaders(), "[method, remove, set, times, url]", "REQUEST_INTERCEPTION", "url", "*/x", "set", "A=1"),
						new Case(new ClearRequestRules(), "none", "REQUEST_INTERCEPTION"),
						new Case(new CheckRequests(), "[atLeast, atMost, clear, exactly, failed, ignoreServiceWorkerCopies, matching, waitMillis]",
									"REQUEST_LOG", "matching", "x"),
						new Case(new RequestLog(), "[clear, matching]", "REQUEST_LOG"),
						new Case(new CheckDialog(), "[expected, type]", "DIALOGS"),
						new Case(new DeleteCookies(), "[names]", "COOKIES", "names", "a"),
						new Case(new LowerQuota(), "[headroomKb, restore]", "STORAGE_QUOTA", "headroomKb", "1"),
						new Case(new BlockIndexedDb(), "none", "INIT_SCRIPTS"),
						new Case(new ShiftClock(), "[days]", "INIT_SCRIPTS", "days", "1"),
						new Case(new PressKey(), "[count, key]", "KEYS", "key", "ESCAPE"),
						new Case(new ResizeWindow(), "[height, width]", "WINDOW", "width", "800", "height", "600"),
						new Case(new FreezeTab(), "[action, index]", "PAGE_LIFECYCLE", "action", "freeze", "index", "1"),
						new Case(new SwitchTab(), "[action, index, url]", "TABS", "action", "close"),
						new Case(new CheckOpenedTab(), "[close, script, timeoutMillis]", "TABS", "script", "return true;"),
						new Case(new StartDevice(), "[offline, profile, remembered, touch]", "a device profile", "profile", "phone1"),
						new Case(new RestartDevice(), "[offline, touch]", "a device profile"));
	}

	/** An unknown parameter is refused before the call does anything, naming the call and the names it takes. */
	@Test
	void everyCallRefusesAParameterItDoesNotTake() {
		FakeBrowserControl browser = FakeBrowserControl.supportingAll();
		AbstractSailCall.Context context = context(browser);
		assertAll(cases().stream().map(c -> (Executable) () -> {
			Map<String, String> given = parameters(c.valid());
			given.put("bogus", "1");
			AbstractSailCall call = c.call();
			IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> call.call(given, context), c.name());
			assertEquals(c.name() + " doesn't take the parameter [bogus]: it takes " + c.takes(), e.getMessage(), c.name());
		}));
		assertEquals(List.of(), browser.calls(), "nothing was done in the browser");
	}

	/**
	 * Unsupported means skipped: in a browser without the capability a call needs (or without profiles, for the device
	 * calls), it aborts the test, naming itself, what it needs and the browser, before it does anything.
	 */
	@Test
	void everyCallSkipsInABrowserThatLacksWhatItNeeds() {
		FakeBrowserControl firefox = new FakeBrowserControl("firefox");
		WebDriver driver = mock(WebDriver.class, withSettings().extraInterfaces(JavascriptExecutor.class));
		PrimeFacesSelenide selenide = mockSelenide();
		AbstractSailCall.Context context = context(() -> driver, selenide, d -> firefox);
		assertAll(Stream.concat(cases().stream().map(c -> (Executable) () -> {
									AbstractSailCall call = c.call();
									String[] valid = c.valid();
									TestAbortedException e = assertThrows(TestAbortedException.class, () -> run(call, context, valid), c.name());
									assertEquals(c.name() + " needs " + c.needs() + ", which firefox doesn't support", e.getMessage(), c.name());
									if (! c.needs().startsWith("a ")) {
										assertSame(SailCapabilityUnavailableException.class, e.getClass(), c.name());
									}
								}),
								Stream.of(() -> assertEquals(List.of(), firefox.calls(), "nothing was done in the browser"),
											() -> verifyNoInteractions(driver),
											() -> verify(selenide, never()).stopBrowser())));
	}

	/** A call runs in a browser: under any other executor it says so, after checking its parameters. */
	@Test
	void aCallNeedsABrowser() {
		Executor executor = mock(Executor.class);
		Offline offline = new Offline();
		Map<String, String> none = Map.of();
		Map<String, String> unknownParameter = Map.of("x", "1");
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> offline.call(none, executor));
		IllegalArgumentException unknown = assertThrows(IllegalArgumentException.class, () -> offline.call(unknownParameter, executor));
		assertAll(() -> assertTrue(e.getMessage().startsWith("Offline needs a browser, but runs under Executor"), e.getMessage()),
					() -> assertEquals("Offline doesn't take the parameter [x]: it takes none", unknown.getMessage(), "the parameters first"));
	}

	/** The leg gives the browser running now, and its control, so both follow a restart. */
	@Test
	void theLegGivesTheBrowserRunningNow() {
		WebDriver first = mock(WebDriver.class);
		WebDriver second = mock(WebDriver.class);
		WebDriver[] running = {first};
		PrimeFacesSelenide selenide = mockSelenide();
		AbstractSailCall.Context context = context(() -> running[0], selenide, SailBrowserControl::of);
		SailBrowserControl before = context.control();
		running[0] = second;
		assertAll(() -> assertSame(second, context.driver()),
					() -> assertSame(SailBrowserControl.of(second), context.control()),
					() -> assertSame(SailBrowserControl.of(first), before),
					() -> assertSame(selenide, context.selenide()),
					() -> assertEquals(BASE_URL, context.baseUrl()),
					() -> assertEquals(TIMEOUT, context.timeout()),
					() -> assertEquals("run1", context.runToken()));
	}
}
