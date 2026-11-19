package util.sail.browser;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.ImmutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.remote.SessionId;
import org.opentest4j.TestAbortedException;

/**
 * Tests the entry point of browser control without a browser: one control per driver until it quits, what plain
 * WebDriver supports, and that a capability the browser lacks skips the test, naming the step, the capability and the
 * browser. The drivers are mocks.
 */
@SuppressWarnings("static-method")
class SailBrowserControlTest {
	/** The capabilities plain WebDriver has. */
	private static final Set<Capability> WEB_DRIVER = EnumSet.of(Capability.COOKIES, Capability.TABS, Capability.KEYS, Capability.WINDOW);

	/** Returns a driver with a browser name, as a real driver's capabilities give it. */
	private static WebDriver named(String browserName) {
		WebDriver result = mock(WebDriver.class, withSettings().extraInterfaces(HasCapabilities.class));
		when(((HasCapabilities) result).getCapabilities()).thenReturn(new ImmutableCapabilities("browserName", browserName));
		return result;
	}

	@Test
	void eachDriverHasOneControlUntilItQuits() {
		WebDriver driver = mock(WebDriver.class);
		WebDriver other = mock(WebDriver.class);
		RemoteWebDriver remote = mock(RemoteWebDriver.class);
		when(remote.getSessionId()).thenReturn(new SessionId("s1"));
		SailBrowserControl first = SailBrowserControl.of(remote);
		SailBrowserControl again = SailBrowserControl.of(remote);
		when(remote.getSessionId()).thenReturn(null); // as quit() leaves it
		SailBrowserControl afterQuit = SailBrowserControl.of(remote);
		assertAll(() -> assertSame(SailBrowserControl.of(driver), SailBrowserControl.of(driver), "the same driver"),
					() -> assertNotSame(SailBrowserControl.of(driver), SailBrowserControl.of(other), "another driver"),
					() -> assertSame(first, again, "a remote driver whose session is open"),
					() -> assertNotSame(first, afterQuit, "a driver that has quit: its control was released"));
	}

	/** Chrome and Edge get the Chromium control, which has every capability (a mock driver offers no DevTools to connect to). */
	@Test
	void aChromiumDriverGetsTheChromiumControl() {
		ChromeDriver chrome = mock(ChromeDriver.class);
		when(chrome.getSessionId()).thenReturn(new SessionId("s2"));
		when(chrome.getCapabilities()).thenReturn(new ImmutableCapabilities("browserName", "chrome"));
		SailBrowserControl control = SailBrowserControl.of(chrome);
		assertAll(() -> assertEquals("chrome", control.browser()),
					() -> assertSame(ChromiumBrowserControl.class, control.getClass()),
					() -> assertSame(control, SailBrowserControl.of(chrome), "kept while the session is open"),
					() -> assertAll(Stream.of(Capability.values())
											.map(c -> (Executable) () -> assertTrue(control.supports(c), c.name()))));
	}

	@Test
	void plainWebDriverSupportsCookiesTabsKeysAndWindowOnly() {
		SailBrowserControl control = SailBrowserControl.of(named("firefox"));
		assertAll(Stream.of(Capability.values())
						.map(c -> (Executable) () -> assertEquals(Boolean.valueOf(WEB_DRIVER.contains(c)), Boolean.valueOf(control.supports(c)), c.name())));
	}

	@Test
	void requireGivesTheInterfaceOfASupportedCapability() {
		SailBrowserControl control = SailBrowserControl.of(named("firefox"));
		assertAll(() -> assertSame(control, control.require(Cookies.class, "DeleteCookies")),
					() -> assertSame(control, control.require(Tabs.class, "SwitchTab")),
					() -> assertSame(control, control.require(Keys.class, "PressKey")),
					() -> assertSame(control, control.require(Window.class, "ResizeWindow")));
	}

	/** Unsupported means skipped: the exception is JUnit's abort, and names the step, the capability and the browser. */
	@Test
	void requireSkipsTheTestForAnUnsupportedCapability() {
		SailBrowserControl control = SailBrowserControl.of(named("firefox"));
		TestAbortedException e = assertThrows(TestAbortedException.class, () -> control.require(NetworkConditions.class, "Offline"),
												"JUnit reports it as skipped");
		assertAll(() -> assertEquals("Offline needs NETWORK_CONDITIONS, which firefox doesn't support", e.getMessage()),
					() -> assertSame(SailCapabilityUnavailableException.class, e.getClass()));
	}

	@Test
	void aDriverWithoutABrowserNameIsNamedByItsClass() {
		RemoteWebDriver unnamed = mock(RemoteWebDriver.class);
		when(unnamed.getSessionId()).thenReturn(new SessionId("s3"));
		when(unnamed.getCapabilities()).thenReturn(new ImmutableCapabilities());
		assertAll(() -> assertEquals("chrome", SailBrowserControl.browserName(named("chrome"))),
					() -> assertEquals(unnamed.getClass().getSimpleName(), SailBrowserControl.browserName(unnamed), "no browser name"),
					() -> assertEquals("WebDriver", SailBrowserControl.browserName(mock(WebDriver.class)).replaceFirst("\\$.*", ""), "no capabilities"));
	}

	@Test
	void eachCapabilityHasItsOwnInterface() {
		List<Executable> checks = Stream.of(Capability.values())
										.map(c -> (Executable) () -> assertSame(c, Capability.of(c.type()), c.name()))
										.toList();
		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> Capability.of(String.class));
		assertAll(Stream.concat(checks.stream(),
								Stream.of(() -> assertEquals("java.lang.String is not the interface of a browser capability", e.getMessage()))));
	}

	/** The fake that step tests use supports what the test chooses, skips the rest, and records what it is asked. */
	@Test
	void theFakeSupportsWhatTheTestChoosesAndRecordsItsCalls() {
		FakeBrowserControl fake = new FakeBrowserControl("firefox", Capability.COOKIES)
										.withCookies(new BrowserCookie("remember", "1", "h", "/", false),
														new BrowserCookie("JSESSIONID", "2", "h", "/", true));
		int deleted = fake.require(Cookies.class, "DeleteCookies").deleteCookies("remember");
		List<String> left = fake.cookies().stream().map(BrowserCookie::name).toList();
		SailCapabilityUnavailableException e = assertThrows(SailCapabilityUnavailableException.class,
															() -> fake.require(RequestLog.class, "CheckRequests"));
		FakeBrowserControl all = FakeBrowserControl.supportingAll();
		all.require(NetworkConditions.class, "Offline").offline();
		all.require(Keys.class, "PressKey").press(org.openqa.selenium.Keys.CONTROL, "a");
		assertAll(() -> assertEquals(1, deleted),
					() -> assertEquals(List.of("JSESSIONID"), left),
					() -> assertEquals(List.of("deleteCookies remember", "cookies"), fake.calls()),
					() -> assertEquals("CheckRequests needs REQUEST_LOG, which firefox doesn't support", e.getMessage()),
					() -> assertEquals(List.of("offline", "press CONTROL+a"), all.calls()),
					() -> assertTrue(Stream.of(Capability.values()).allMatch(all::supports), "every capability"));
	}
}
