package util.sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.WebDriver;
import org.junit.jupiter.api.function.Executable;

import util.sail.BrowserConfiguration.Browsers;

/**
 * Tests the device profile and extra browser arguments of {@link BrowserConfiguration}, the arguments that
 * {@link CrossBrowserSelenium} builds from them, and how the workflow tells two devices apart, without a browser.
 */
class DeviceProfileTest {
	private static final Path DIR = Path.of("target", "device-profile-test", "phone-1").toAbsolutePath();
	private static final String DATA_DIR = "--user-data-dir=" + DIR;
	private static final String FAKE = "--use-fake-device-for-media-stream";
	private static final List<String> CHROME_BUILT_IN = List.of("--disable-backgrounding-occluded-windows",
																"--disable-renderer-backgrounding",
																"--disable-background-timer-throttling");

	@Test
	@SuppressWarnings("static-method")
	void theProfileIsAbsoluteAndTheArgumentsAreAppendedInOrder() {
		BrowserConfiguration configuration = new BrowserConfiguration().profile(Path.of("target", "p")).arguments("--a").arguments("--b", "--c");
		BrowserConfiguration none = new BrowserConfiguration();
		List<String> arguments = configuration.getArguments();
		assertAll(() -> assertEquals(Path.of("target", "p").toAbsolutePath(), configuration.getProfile()),
					() -> assertEquals(List.of("--a", "--b", "--c"), arguments),
					() -> assertThrows(UnsupportedOperationException.class, () -> arguments.add("--d")),
					() -> assertNull(none.getProfile(), "no profile by default"),
					() -> assertTrue(none.getArguments().isEmpty(), "no arguments by default"),
					() -> assertNull(configuration.profile(null).getProfile(), "null clears the profile"));
	}

	@Test
	@SuppressWarnings("static-method")
	void anArgumentThatIsNullOrSetsTheProfileIsRefused() {
		BrowserConfiguration configuration = new BrowserConfiguration();
		assertAll(() -> assertThrows(NullPointerException.class, () -> configuration.arguments("--a", null), "null"),
					() -> assertThrows(IllegalArgumentException.class, () -> configuration.arguments("--user-data-dir=/x"), "user data dir"));
	}

	@Test
	@SuppressWarnings("static-method")
	void chromeAndEdgeArgumentsHaveTheProfileThenTheExtrasAfterTheBuiltInOnes() {
		BrowserConfiguration plain = new BrowserConfiguration();
		BrowserConfiguration full = new BrowserConfiguration().userAgentString("UA").profile(DIR).arguments(FAKE, "--x");
		BrowserConfiguration extrasOnly = new BrowserConfiguration().arguments(FAKE);
		List<String> chromeHeaded = CrossBrowserSelenium.chromeArguments(plain, false);
		List<String> chromeFull = CrossBrowserSelenium.chromeArguments(full, true);
		assertAll(() -> assertEquals(CHROME_BUILT_IN, chromeHeaded, "chrome, plain"),
					() -> assertEquals(List.of(CHROME_BUILT_IN.get(0), CHROME_BUILT_IN.get(1), CHROME_BUILT_IN.get(2),
												"--user-agent=UA", "--headless", "--window-size=1920,1080", DATA_DIR, FAKE, "--x"),
										chromeFull, "chrome, profile and extras"),
					() -> assertEquals(List.of(CHROME_BUILT_IN.get(0), CHROME_BUILT_IN.get(1), CHROME_BUILT_IN.get(2), FAKE),
										CrossBrowserSelenium.chromeArguments(extrasOnly, false), "chrome, extras only"),
					() -> assertEquals(List.of(CHROME_BUILT_IN.get(0), CHROME_BUILT_IN.get(1), CHROME_BUILT_IN.get(2), "--headless"),
										CrossBrowserSelenium.chromeArguments(new BrowserConfiguration().resolution(10, 20), true), "chrome, headless with its own size"),
					() -> assertEquals(List.of(), CrossBrowserSelenium.edgeArguments(plain, false), "edge, plain"),
					() -> assertEquals(List.of("--user-agent=UA", "headless", DATA_DIR, FAKE, "--x"),
										CrossBrowserSelenium.edgeArguments(full, true), "edge, profile and extras"),
					() -> assertEquals(List.of(FAKE), CrossBrowserSelenium.edgeArguments(extrasOnly, false), "edge, extras only"));
	}

	/** Firefox and Safari refuse before any browser starts: the real start throws, so no driver is made. */
	@Test
	@SuppressWarnings("static-method")
	void firefoxAndSafariRefuseAProfileOrExtraArguments() {
		assertAll(refused(Browsers.firefox, "profile", new BrowserConfiguration().browser(Browsers.firefox).fixed().profile(DIR)),
					refused(Browsers.firefox, "arguments", new BrowserConfiguration().browser(Browsers.firefox).fixed().arguments(FAKE)),
					refused(Browsers.safari, "profile", new BrowserConfiguration().browser(Browsers.safari).fixed().profile(DIR)),
					refused(Browsers.safari, "arguments", new BrowserConfiguration().browser(Browsers.safari).fixed().arguments(FAKE)));
		assertAll(() -> CrossBrowserSelenium.requireProfileSupport(Browsers.firefox, new BrowserConfiguration()),
					() -> CrossBrowserSelenium.requireProfileSupport(Browsers.chrome, new BrowserConfiguration().profile(DIR).arguments(FAKE)),
					() -> CrossBrowserSelenium.requireProfileSupport(Browsers.edge, new BrowserConfiguration().profile(DIR).arguments(FAKE)));
	}

	private static Executable refused(Browsers browser, String what, BrowserConfiguration configuration) {
		return () -> {
			IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
														() -> new PrimeFacesSelenide().startBrowser(configuration),
														browser + " with " + what);
			assertTrue(e.getMessage().contains("Chrome and Edge") && e.getMessage().contains(browser.name()), browser + " with " + what + ": " + e.getMessage());
		};
	}

	/** Two configurations are the same device only if their profile and extra arguments match as well. */
	@Test
	@SuppressWarnings("static-method")
	void aDifferentProfileOrArgumentsIsAnotherDevice() {
		BrowserConfiguration a = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/").profile(DIR).arguments(FAKE);
		SailSettings resolved = a.resolve(SailSettings.Environment.none());
		PrimeFacesSelenide selenide = started(a, resolved);
		BrowserConfiguration same = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/").profile(DIR).arguments(FAKE);
		BrowserConfiguration otherProfile = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/").profile(DIR.resolveSibling("phone-2")).arguments(FAKE);
		BrowserConfiguration noProfile = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/").arguments(FAKE);
		BrowserConfiguration otherArguments = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/").profile(DIR).arguments("--x");
		assertAll(() -> assertNull(selenide.browserChange(resolved, same), "identical"),
					() -> assertEquals("profile " + DIR + " -> " + DIR.resolveSibling("phone-2"), selenide.browserChange(resolved, otherProfile)),
					() -> assertEquals("profile " + DIR + " -> null", selenide.browserChange(resolved, noProfile)),
					() -> assertEquals("arguments [" + FAKE + "] -> [--x]", selenide.browserChange(resolved, otherArguments)));
	}

	/** The Selenide follows a browser that a step restarted: the next leg compares against what it started with last. */
	@Test
	@SuppressWarnings("static-method")
	void aSelenideThatDrivesAnotherSelenidesBrowserTakesItsProfileAndArguments() {
		BrowserConfiguration a = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/").profile(DIR).arguments(FAKE);
		SailSettings resolved = a.resolve(SailSettings.Environment.none());
		PrimeFacesSelenide first = started(a, resolved);
		first.driver = mock(WebDriver.class);
		SmartClientSelenide second = new SmartClientSelenide();
		second.useBrowserOf(first);
		BrowserConfiguration plain = new BrowserConfiguration().baseUrl("http://localhost:1/skyve/");
		assertAll(() -> assertNull(second.browserChange(resolved, a)),
					() -> assertEquals("profile " + DIR + " -> null, arguments [" + FAKE + "] -> []", second.browserChange(resolved, plain)));
	}

	/** Records a configuration as started, as {@code startBrowser} does, without a browser. */
	private static PrimeFacesSelenide started(BrowserConfiguration configuration, SailSettings resolved) {
		PrimeFacesSelenide result = new PrimeFacesSelenide();
		result.recordStart(configuration, resolved);
		return result;
	}
}
